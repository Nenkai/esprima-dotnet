using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Globalization;
using System.Numerics;
using System.Runtime.CompilerServices;
using System.Text;
using System.Text.RegularExpressions;
using Esprima.Ast;

namespace Esprima;

internal readonly struct ScannerState
{
    public readonly int Index;
    public readonly int LineNumber;
    public readonly int LineStart;
    public readonly ArrayList<string> CurlyStack;

    public ScannerState(int index, int lineNumber, int lineStart, ArrayList<string> curlyStack)
    {
        Index = index;
        LineNumber = lineNumber;
        LineStart = lineStart;
        CurlyStack = curlyStack;
    }
}

internal readonly record struct LexOptions(bool Strict, bool AllowIdentifierEscape)
{
    public LexOptions(AdhocAbstractSyntaxTree.Context context) : this(context.Strict, context.AllowIdentifierEscape)
    {
    }
}

public sealed partial class Scanner
{
    internal const int NonIdentifierInterningThreshold = 20;

    private readonly ErrorHandler _errorHandler;
    private readonly bool _tolerant;
    private readonly bool _trackComment;

    private int _length;
    internal string _source; // should be named _code to match the corresponding ctor parameter name but internally we keep this name to match Esprima.org naming
    internal string? _sourceLocation; // should be named _source to match the corresponding ctor parameter name

    internal int _index;
    internal int _lineNumber;
    internal int _lineStart;

    internal bool _isModule;

    private ArrayList<string> _curlyStack;
    private readonly StringBuilder _sb = new();

    internal StringPool _stringPool;
    internal CodePointRange.Cache? _codePointRangeCache;

    private static int OctalValue(char ch)
    {
        return ch - '0';
    }

    internal Scanner(ScannerOptions options)
    {
        if (options == null)
        {
            throw new ArgumentNullException(nameof(options));
        }

        _errorHandler = options.ErrorHandler;
        _tolerant = options.Tolerant;
        _trackComment = options.Comments;

        _source = string.Empty;
    }

    public Scanner(string code) : this(code, ScannerOptions.Default)
    {
    }

    public Scanner(string code, ScannerOptions options) : this(code, null, options)
    {
    }

    public Scanner(string code, string? source) : this(code, source, ScannerOptions.Default)
    {
    }

    public Scanner(string code, string? source, ScannerOptions options) : this(options)
    {
        Reset(code, source);
    }

    public void Reset()
    {
        Reset(startIndex: 0, lineNumber: _length > 0 ? 1 : 0, lineStartIndex: 0);
    }

    public void Reset(int startIndex, int lineNumber, int lineStartIndex)
    {
        if (_length > 0)
        {
            _index = 0 <= startIndex && startIndex < _length ? startIndex : throw new ArgumentOutOfRangeException(nameof(startIndex));
            _lineNumber = lineNumber > 0 ? lineNumber : throw new ArgumentOutOfRangeException(nameof(lineNumber));
            _lineStart = 0 <= lineStartIndex && lineStartIndex <= _index ? lineStartIndex : throw new ArgumentOutOfRangeException(nameof(lineStartIndex));
        }
        else
        {
            _index = startIndex == 0 ? startIndex : throw new ArgumentOutOfRangeException(nameof(startIndex));
            _lineNumber = lineNumber == 0 ? lineNumber : throw new ArgumentOutOfRangeException(nameof(lineNumber));
            _lineStart = lineStartIndex == 0 ? lineStartIndex : throw new ArgumentOutOfRangeException(nameof(lineStartIndex));
        }

        _curlyStack.Clear();
        _sb.Clear();

        _errorHandler.Reset();
    }

    internal void Reset(string code, string? source)
    {
        Reset(code, source, startIndex: 0, lineNumber: code.Length > 0 ? 1 : 0, lineStartIndex: 0);
    }

    internal void Reset(string code, string? source, int startIndex, int lineNumber, int lineStartIndex)
    {
        _source = code ?? throw new ArgumentNullException(nameof(code));
        _length = code.Length;
        _sourceLocation = source;

        Reset(startIndex, lineNumber, lineStartIndex);
        _stringPool = default;
        _codePointRangeCache = null;
        _isModule = false;
    }

    public void SetFileName(string fileName)
    {
        _sourceLocation = fileName;
    }

    internal void ReleaseLargeBuffers()
    {
        _curlyStack.Clear();
        if (_curlyStack.Capacity > 16)
        {
            _curlyStack.Capacity = 16;
        }

        _sb.Clear();
        if (_sb.Capacity > 1024)
        {
            _sb.Capacity = 1024;
        }

        _stringPool = default;
        _codePointRangeCache = null;
    }

    public string Code { [MethodImpl(MethodImplOptions.AggressiveInlining)] get => _source; }

    public int Index { [MethodImpl(MethodImplOptions.AggressiveInlining)] get => _index; }
    public int LineNumber { [MethodImpl(MethodImplOptions.AggressiveInlining)] get => _lineNumber; }
    public int LineStart { [MethodImpl(MethodImplOptions.AggressiveInlining)] get => _lineStart; }

    internal ScannerState SaveState()
    {
        return new ScannerState(_index, _lineNumber, _lineStart, new ArrayList<string>(_curlyStack.ToArray()));
    }

    internal void RestoreState(in ScannerState state)
    {
        _index = state.Index;
        _lineNumber = state.LineNumber;
        _lineStart = state.LineStart;
        _curlyStack = state.CurlyStack;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal bool Eof()
    {
        return _index >= _length;
    }

    private ParseError TolerateUnexpectedToken(string message = Messages.UnexpectedTokenIllegal)
    {
        return _errorHandler.TolerateError(_sourceLocation, _index, _lineNumber, _index - _lineStart, message, _tolerant);
    }

    private StringBuilder GetStringBuilder()
    {
        _sb.Clear();
        return _sb;
    }

    [StringMatcher("&&", "||", "==", "!=", "+=", "-=", "*=", "/=", "++", "--", "<<", ">>", "&=", "|=", "^=", "%=", "<=", ">=", "=>", "**", "::")]
    private static partial string? TryGetInternedTwoCharacterPunctuator(ReadOnlySpan<char> id);

    [StringMatcher("<<=", ">>=", "**=", "&&=", "||=")]
    private static partial string? TryGetInternedThreeCharacterPunctuator(ReadOnlySpan<char> id);

    // https://tc39.github.io/ecma262/#sec-keywords

    // Note for maintainers: all keywords listed here should be included in ParserExtensions.TryGetInternedString too!
    [StringMatcher(
        "if", "do", "var", "for", "foreach", "static", "attribute", "try", "self", "else", "case", "while", "break", "catch", "throw",
        "yield", "class", "module", "super", "return", "switch", "import", "default", "finally", "function", "method",
        "continue", "undef", "print", "delegate", "require")]
    public static partial bool IsKeyword(string id);

    // https://tc39.github.io/ecma262/#sec-comments

    private ArrayList<Comment> SkipSingleLineComment(int offset)
    {
        var comments = new ArrayList<Comment>();
        var start = 0;
        Position startPosition = default, endPosition;
        Esprima.Range slice;

        if (_trackComment)
        {
            start = _index - offset;
            startPosition = new Position(_lineNumber, _index - _lineStart - offset);
        }

        while (!Eof())
        {
            var ch = _source[_index];
            ++_index;
            if (Character.IsLineTerminator(ch))
            {
                if (_trackComment)
                {
                    endPosition = new Position(_lineNumber, _index - _lineStart - 1);
                    slice = new Esprima.Range(start + offset, _index - 1);
                    var entry = new Comment
                    (
                        type: CommentType.Line,
                        slice,
                        start: start,
                        end: _index - 1,
                        startPosition,
                        endPosition
                    );

                    comments.Add(entry);
                }

                if (ch == 13 && _source.CharCodeAt(_index) == 10)
                {
                    ++_index;
                }

                ++_lineNumber;
                _lineStart = _index;
                return comments;
            }
        }

        if (_trackComment)
        {
            endPosition = new Position(_lineNumber, _index - _lineStart);
            slice = new Esprima.Range(start + offset, _index);
            var entry = new Comment
            (
                type: CommentType.Line,
                slice,
                start: start,
                end: _index,
                startPosition,
                endPosition
            );

            comments.Add(entry);
        }

        return comments;
    }

    private ArrayList<Comment> SkipMultiLineComment()
    {
        var comments = new ArrayList<Comment>();
        var start = 0;
        Position startPosition = default, endPosition;
        Esprima.Range slice;

        if (_trackComment)
        {
            start = _index - 2;
            startPosition = new Position(_lineNumber, _index - _lineStart - 2);
        }

        while (!Eof())
        {
            var ch = _source[_index];
            if (Character.IsLineTerminator(ch))
            {
                if (ch == 0x0D && _source.CharCodeAt(_index + 1) == 0x0A)
                {
                    ++_index;
                }

                ++_lineNumber;
                ++_index;
                _lineStart = _index;
            }
            else if (ch == 0x2A)
            {
                // Block comment ends with '*/'.
                if (_source.CharCodeAt(_index + 1) == 0x2F)
                {
                    _index += 2;
                    if (_trackComment)
                    {
                        endPosition = new Position(_lineNumber, _index - _lineStart);
                        slice = new Esprima.Range(start + 2, _index - 2);
                        var entry = new Comment
                        (
                            type: CommentType.Block,
                            slice,
                            start: start,
                            end: _index,
                            startPosition,
                            endPosition
                        );
                        comments.Add(entry);
                    }

                    return comments;
                }

                ++_index;
            }
            else
            {
                ++_index;
            }
        }

        // Ran off the end of the file - the whole thing is a comment
        if (_trackComment)
        {
            endPosition = new Position(_lineNumber, _index - _lineStart);
            slice = new Esprima.Range(start + 2, _index);
            var entry = new Comment
            (
                type: CommentType.Block,
                slice,
                start: start,
                end: _index,
                startPosition,
                endPosition
            );
            comments.Add(entry);
        }

        TolerateUnexpectedToken();
        return comments;
    }

    internal ArrayList<Comment> ScanCommentsInternal()
    {
        var comments = new ArrayList<Comment>();

        var start = _index == 0;
        while (!Eof())
        {
            var ch = _source[_index];

            if (Character.IsWhiteSpace(ch))
            {
                ++_index;
            }
            else if (Character.IsLineTerminator(ch))
            {
                ++_index;
                if (ch == 0x0D && _source.CharCodeAt(_index) == 0x0A)
                {
                    ++_index;
                }

                ++_lineNumber;
                _lineStart = _index;
                start = true;
            }
            else if (ch == 0x2F)
            {
                // U+002F is '/'
                ch = _source.CharCodeAt(_index + 1);
                if (ch == 0x2F)
                {
                    _index += 2;
                    var comment = SkipSingleLineComment(2);
                    if (_trackComment)
                    {
                        comments.AddRange(comment.AsReadOnlySpan());
                    }

                    start = true;
                }
                else if (ch == 0x2A)
                {
                    // U+002A is '*'
                    _index += 2;
                    var comment = SkipMultiLineComment();
                    if (_trackComment)
                    {
                        comments.AddRange(comment.AsReadOnlySpan());
                    }
                }
                else
                {
                    break;
                }
            }
            else if (start && ch == 0x2D)
            {
                // U+002D is '-'
                // U+003E is '>'
                if (_source.CharCodeAt(_index + 1) == 0x2D && _source.CharCodeAt(_index + 2) == 0x3E)
                {
                    // '-->' is a single-line comment
                    _index += 3;
                    var comment = SkipSingleLineComment(3);
                    if (_trackComment)
                    {
                        comments.AddRange(comment.AsReadOnlySpan());
                    }
                }
                else
                {
                    break;
                }
            }
            else if (ch == 0x3C)
            {
                // U+003C is '<'
                if (_source.CharCodeAt(_index + 1) == '!'
                    && _source.CharCodeAt(_index + 2) == '-'
                    && _source.CharCodeAt(_index + 3) == '-')
                {
                    if (_isModule)
                    {
                        TolerateUnexpectedToken();
                    }

                    _index += 4; // `<!--`
                    var comment = SkipSingleLineComment(4);
                    if (_trackComment)
                    {
                        comments.AddRange(comment.AsReadOnlySpan());
                    }
                }
                else
                {
                    break;
                }
            }
            else
            {
                break;
            }
        }

        return comments;
    }

    public ReadOnlySpan<Comment> ScanComments()
    {
        var comments = ScanCommentsInternal();
        comments.TrimExcess();
        return comments.AsReadOnlySpan();
    }

    private bool ScanHexEscape(char prefix, out char result)
    {
        var len = prefix == 'u' ? 4 : 2;
        var code = 0;

        for (var i = 0; i < len; ++i)
        {
            if (!Eof())
            {
                var d = _source[_index];
                if (Character.IsHexDigit(d))
                {
                    code = code * 16 + HexConverter.FromChar(d);
                    _index++;
                }
                else
                {
                    result = char.MinValue;
                    return false;
                }
            }
            else
            {
                result = char.MinValue;
                return false;
            }
        }

        result = (char) code;
        return true;
    }

    private bool TryScanUnicodeCodePointEscape(out int code)
    {
        var ch = _source.CharCodeAt(_index);
        code = 0;

        // At least, one hex digit is required.
        if (ch == '}')
        {
            return false;
        }

        while (!Eof())
        {
            ch = _source[_index++];
            if (!Character.IsHexDigit(ch))
            {
                break;
            }

            code = code * 16 + HexConverter.FromChar(ch);

            // The Unicode standard guarantees that a code point above 0x10FFFF will never be assigned
            // (see https://stackoverflow.com/a/52203901/8656352).
            if (code > Character.UnicodeLastCodePoint)
            {
                return false;
            }
        }

        if (ch != '}')
        {
            return false;
        }

        // The surrogate range is valid in literals (e.g. "a\u{d800}\u{dc00}") but not valid in identifiers (e.g. a\u{d800}\u{dc00}).
        // Let's return true in both cases and let the caller deal with it.

        return true;
    }

    private int ScanUnicodeCodePointEscape()
    {
        if (!TryScanUnicodeCodePointEscape(out var cp))
        {
            TolerateUnexpectedToken(cp > Character.UnicodeLastCodePoint
                ? Messages.UndefinedUnicodeCodePoint
                : Messages.InvalidUnicodeEscapeSequence);
        }

        return cp;
    }

    private string GetIdentifier(bool allowEscapedSurrogates = false)
    {
        var start = _index++;
        while (!Eof())
        {
            var ch = _source[_index];
            if ((ushort) ch is
                0x5C // Blackslash (U+005C) marks Unicode escape sequence.
                or >= 0xD800 and <= 0xDFFF) // Need to handle surrogate pairs. 
            {
                _index = start;
                return GetComplexIdentifier(allowEscapedSurrogates);
            }

            if (Character.IsIdentifierPart(ch))
            {
                ++_index;
            }
            else
            {
                break;
            }
        }

        return _source.Between(start, _index).ToInternedString(ref _stringPool);
    }

    private string GetComplexIdentifier(bool allowEscapedSurrogates = false)
    {
        var sb = GetStringBuilder();

        var cp = _source.CodePointAt(_index);
        int chcp;

        if (cp != 0x5C)
        {
            if (cp <= char.MaxValue)
            {
                if (char.IsSurrogate((char) cp))
                {
                    TolerateUnexpectedToken();
                }

                sb.Append((char) cp);
            }
            else
            {
                if (!Character.IsIdentifierStartAstral(cp))
                {
                    TolerateUnexpectedToken();
                }

                sb.AppendCodePoint(cp);
                _index++;
            }
            _index++;
        }
        else
        {
            // '\u' (U+005C, U+0075) denotes an escaped character.
            ++_index;
            if (_source.CharCodeAt(_index) != 0x75)
            {
                TolerateUnexpectedToken();
            }

            ++_index;
            if (_source.CharCodeAt(_index) == '{')
            {
                ++_index;
                if (!TryScanUnicodeCodePointEscape(out chcp))
                {
                    TolerateUnexpectedToken(chcp > Character.UnicodeLastCodePoint
                        ? Messages.UndefinedUnicodeCodePoint
                        : Messages.InvalidUnicodeEscapeSequence);
                }
            }
            else
            {
                if (!ScanHexEscape('u', out var ch1) || ch1 == '\\')
                {
                    TolerateUnexpectedToken();
                }

                if (!char.IsSurrogate(ch1))
                {
                    if (!Character.IsIdentifierStart(ch1))
                    {
                        TolerateUnexpectedToken();
                    }

                    sb.Append(ch1);
                    goto ParseIdentifierPart;
                }
                else if (!allowEscapedSurrogates || !char.IsHighSurrogate(ch1) || !TryGetEscapedSurrogate(ch1, out chcp))
                {
                    TolerateUnexpectedToken();
                    chcp = default; // keeps the compiler happy
                }
            }

            if (chcp > char.MaxValue)
            {
                if (!Character.IsIdentifierStartAstral(chcp))
                {
                    TolerateUnexpectedToken();
                }
                sb.AppendCodePoint(chcp);
            }
            else
            {
                if (char.IsSurrogate((char) chcp) || !Character.IsIdentifierStart((char) chcp))
                {
                    TolerateUnexpectedToken();
                }
                sb.Append((char) chcp);
            }
        }

ParseIdentifierPart:
        while (!Eof())
        {
            cp = _source.CodePointAt(_index);

            if (cp != 0x5C)
            {
                if (cp <= char.MaxValue)
                {
                    // IsIdentifierPart also matches the surrogate range (U+D800..U+DFFF).
                    if (!Character.IsIdentifierPart((char) cp))
                    {
                        break;
                    }
                    else if (char.IsSurrogate((char) cp))
                    {
                        TolerateUnexpectedToken();
                    }

                    sb.Append((char) cp);
                }
                else
                {
                    if (!Character.IsIdentifierPartAstral(cp))
                    {
                        break;
                    }

                    sb.AppendCodePoint(cp);
                    _index++;
                }
                _index++;
            }
            else
            {
                // '\u' (U+005C, U+0075) denotes an escaped character.
                ++_index;
                if (_source.CharCodeAt(_index) != 0x75)
                {
                    TolerateUnexpectedToken();
                }

                ++_index;
                if (_source.CharCodeAt(_index) == '{')
                {
                    ++_index;
                    if (!TryScanUnicodeCodePointEscape(out chcp))
                    {
                        TolerateUnexpectedToken(chcp > Character.UnicodeLastCodePoint
                            ? Messages.UndefinedUnicodeCodePoint
                            : Messages.InvalidUnicodeEscapeSequence);
                    }
                }
                else
                {
                    if (!ScanHexEscape('u', out var ch1) || ch1 == '\\')
                    {
                        TolerateUnexpectedToken();
                    }

                    if (!char.IsSurrogate(ch1))
                    {
                        if (!Character.IsIdentifierPart(ch1))
                        {
                            TolerateUnexpectedToken();
                        }

                        sb.Append(ch1);
                        continue;
                    }
                    else if (!allowEscapedSurrogates || !char.IsHighSurrogate(ch1) || !TryGetEscapedSurrogate(ch1, out chcp))
                    {
                        TolerateUnexpectedToken();
                        chcp = default; // keeps the compiler happy
                    }
                }

                if (chcp > char.MaxValue)
                {
                    if (!Character.IsIdentifierPartAstral(chcp))
                    {
                        TolerateUnexpectedToken();
                    }
                    sb.AppendCodePoint(chcp);
                }
                else
                {
                    if (char.IsSurrogate((char) chcp) || !Character.IsIdentifierPart((char) chcp))
                    {
                        TolerateUnexpectedToken();
                    }
                    sb.Append((char) chcp);
                }
            }
        }

        return sb.ToString().AsSpan().ToInternedString(ref _stringPool);
    }

    private bool TryGetEscapedSurrogate(char highSurrogate, out int cp)
    {
        if (_index + 1 < _source.Length && _source[_index] == '\\' && _source[_index + 1] == 'u')
        {
            _index += 2;
            if (ScanHexEscape('u', out var lowSurrogate) && char.IsLowSurrogate(lowSurrogate))
            {
                cp = char.ConvertToUtf32(highSurrogate, lowSurrogate);
                return true;
            }
        }

        cp = default;
        return false;
    }

    private int OctalToDecimal(char ch, out int length)
    {
        var code = OctalValue(ch);
        length = 1;

        if (!Eof() && Character.IsOctalDigit(_source[_index]))
        {
            code = code * 8 + OctalValue(_source[_index++]);
            length++;

            // 3 digits are only allowed when string starts
            // with 0, 1, 2, 3
            if (ch >= '0' && ch <= '3' && !Eof() && Character.IsOctalDigit(_source[_index]))
            {
                code = code * 8 + OctalValue(_source[_index++]);
                length++;
            }
        }

        return code;
    }

    // https://tc39.github.io/ecma262/#sec-names-and-keywords

    private Token ScanIdentifier(bool allowEscapes)
    {
        TokenType type;
        var start = _index;

        // Backslash (U+005C) starts an escaped character.
        var id = (ushort) _source[_index] is 0x5C or (>= 0xD800 and <= 0xDFFF)
            ? GetComplexIdentifier()
            : GetIdentifier();

        // There is no keyword or literal with only one character.
        // Thus, it must be an identifier.
        if (id.Length == 1)
        {
            type = TokenType.Identifier;
        }
        else if (IsKeyword(id))
        {
            type = TokenType.Keyword;
        }
        else if ("nil".Equals(id, StringComparison.Ordinal))
        {
            type = TokenType.NilLiteral;
        }
        else if ("true".Equals(id, StringComparison.Ordinal) || "false".Equals(id, StringComparison.Ordinal))
        {
            type = TokenType.BooleanLiteral;
        }
        else
        {
            type = TokenType.Identifier;
        }

        if (type != TokenType.Identifier && start + id.Length != _index)
        {
            var restore = _index;
            _index = start;
            if (!allowEscapes)
            {
                TolerateUnexpectedToken(Messages.InvalidEscapedReservedWord);
            }
            _index = restore;
        }

        return Token.Create(type, id, start, end: _index, _lineNumber, _lineStart);
    }

    // https://tc39.github.io/ecma262/#sec-punctuators

    private Token ScanPunctuator()
    {
        var start = _index;

        // Check for most common single-character punctuators.
        var c = _source[_index];
        var str = ParserExtensions.CharToString(c);

        switch (c)
        {
            case '(':
                ++_index;
                break;

            case '{':
                _curlyStack.Add("{");
                ++_index;
                break;

            case '\\': // Adhoc: for macro directives
                ++_index;
                break;

            case '.':
                ++_index;

                if (_source.CharCodeAt(_index) == '*') // Adhoc: .*
                {
                    _index++;
                    str = ".*";
                }
                else if (_source.Length >= _index + 2 && _source[_index] == '.' && _source[_index + 1] == '.')
                {
                    // Spread operator: ...
                    _index += 2;
                    str = "...";
                }

                break;

            case '}':
                ++_index;
                if (_curlyStack.Count > 0)
                {
                    _curlyStack.RemoveAt(_curlyStack.Count - 1);
                }

                break;

            case '?':
                ++_index;
                if (_source.CharCodeAt(_index) == '?')
                {
                    ++_index;
                    if (_source.CharCodeAt(_index) == '=')
                    {
                        ++_index;
                        str = "??=";
                    }
                    else
                    {
                        str = "??";
                    }
                }

                if (_source.CharCodeAt(_index) == '.' && !char.IsDigit(_source.CharCodeAt(_index + 1)))
                {
                    // "?." in "foo?.3:0" should not be treated as optional chaining.
                    // See https://github.com/tc39/proposal-optional-chaining#notes
                    ++_index;
                    str = "?.";
                }

                if (_source.CharCodeAt(_index) == '[')
                {
                    ++_index;
                    str = "?[";
                }

                break;

            case ':':
                ++_index;
                if (_source.CharCodeAt(_index) == ':')
                {
                    ++_index;
                    str = "::";
                }
                break;

            case ')':
            case ';':
            case ',':
            case '[':
            case ']':
            case '~':
            case '#': // Adhoc include
            case '@': // Adhoc pragma
                ++_index;
                break;

            default:
                // 3-character punctuators.
                if (_index + 3 <= _source.Length && (str = TryGetInternedThreeCharacterPunctuator(_source.AsSpan(_index, 3))) is not null)
                {
                    _index += 3;
                }
                // 2-character punctuators.
                else if (_index + 2 <= _source.Length && (str = TryGetInternedTwoCharacterPunctuator(_source.AsSpan(_index, 2))) is not null)
                {
                    _index += 2;
                }
                // 1-character punctuators.
                else
                {
                    str = ParserExtensions.CharToString(c);
                    if ("<>=!+-*%&|^/".Contains(c))
                    {
                        ++_index;
                    }
                }

                break;
        }

        if (_index == start)
        {
            TolerateUnexpectedToken();
        }

        return Token.CreatePunctuator(str, start, end: _index, _lineNumber, _lineStart);
    }

    // https://tc39.github.io/ecma262/#sec-literals-numeric-literals

    private Token ScanHexLiteral(int start)
    {
        var number = this.ScanLiteralPart(Character.IsHexDigitFunc, allowNumericSeparator: true);

        if (number.Length == 0)
        {
            TolerateUnexpectedToken();
        }

        var ch = _source.CharCodeAt(_index);

        object value = 0;
        NumericTokenType tokenType = NumericTokenType.None;

        if (_source.CharCodeAt(_index) == 'u' || _source.CharCodeAt(_index) == 'U') // Unsigned
        {
            _index++;
            if (_source.CharCodeAt(_index) == 'l' || _source.CharCodeAt(_index) == 'L') // Unsigned Long
            {
                _index++;
                value = ulong.Parse(number, NumberStyles.HexNumber, CultureInfo.InvariantCulture);
                tokenType = NumericTokenType.UnsignedLong;
            }
            else // UInt
            {
                value = uint.Parse(number, NumberStyles.HexNumber, CultureInfo.InvariantCulture);
                tokenType = NumericTokenType.UnsignedInteger;
            }
        }
        else if (_source.CharCodeAt(_index) == 'l' || _source.CharCodeAt(_index) == 'L') // Long
        {
            _index++;
            value = long.Parse(number, NumberStyles.HexNumber, CultureInfo.InvariantCulture);
            tokenType = NumericTokenType.Long;
        }
        else if (Character.IsIdentifierStart(_source.CharCodeAt(_index)))
        {
            TolerateUnexpectedToken();
        }
        else if (number.Length <= 8)
        {
            value = int.Parse(number, NumberStyles.HexNumber, CultureInfo.InvariantCulture);
            tokenType = NumericTokenType.Integer;
        }
        else if (number.Length <= 16)
        {
            value = long.Parse(number, NumberStyles.HexNumber, CultureInfo.InvariantCulture);
            tokenType = NumericTokenType.Long;
        }
        else if (number.Length > 255)
        {
            value = double.PositiveInfinity;
            tokenType = NumericTokenType.Double;
        }
        else
        {
            double tmpVal = 0;

            double modulo = 1;
            var literal = number.ToString().ToLowerInvariant();
            var length = literal.Length - 1;
            for (var i = length; i >= 0; i--)
            {
                var c = literal[i];

                if (c <= '9')
                {
                    tmpVal += modulo * (c - '0');
                }
                else
                {
                    tmpVal += modulo * (c - 'a' + 10);
                }

                modulo *= 16;
            }

            value = tmpVal;
            tokenType = NumericTokenType.Double;
        }

        return Token.CreateNumericLiteral(value, tokenType, octal: false, start, end: _index, _lineNumber, _lineStart);
    }

    private static double ParseIntAsDouble(ReadOnlySpan<char> number, byte fromBase)
    {
        double value = 0;
        double modulo = 1;
        int i;
        for (i = number.Length; i > 0;)
        {
            var code = number[--i];
            ushort digitValue;

            if (code >= 'a')
            {
                digitValue = (ushort) (code - ('a' - 10));
            }
            else if (code >= 'A')
            {
                digitValue = (ushort) (code - ('A' - 10));
            }
            else if (code is >= '0' and <= '9') // TODO: <= 9 needed?
            {
                digitValue = (ushort) (code - '0');
            }
            else
            {
                Debug.Assert(false, $"Invalid digit in number: U+{(ushort) code:X4}");
                break;
            }

            Debug.Assert(digitValue < fromBase, $"Invalid digit in number: U+{(ushort) code:X4}");

            value += modulo * digitValue;
            modulo *= fromBase;
        }

        Debug.Assert(i == 0, $"Invalid number: {number.ToString()}");

        return value;
    }

    private enum JavaScriptNumberStyle
    {
        Binary,
        Hex,
        Octal,
        Integer
    }

    private Token ScanBinaryLiteral(int start)
    {
        var number = this.ScanLiteralPart(static c => c is '0' or '1', allowNumericSeparator: true);

        if (number.Length == 0)
        {
            // only 0b or 0B
            TolerateUnexpectedToken();
        }

        if (!Eof())
        {
            var ch = _source[_index];
            if (Character.IsIdentifierStart(ch) || Character.IsDecimalDigit(ch))
            {
                TolerateUnexpectedToken();
            }
        }

        object value;
        value = (object) Convert.ToInt32(number.ToString(), 2); // TODO: ToString() no alloc
        // TODO: 64 bit numbers?

        return Token.CreateNumericLiteral(value, NumericTokenType.Integer, octal: false, start, end: _index, _lineNumber, _lineStart);
    }

    private Token ScanOctalLiteral(char prefix, int start, bool isLegacyOctalDigital = false)
    {
        var sb = GetStringBuilder();
        var octal = false;

        if (Character.IsOctalDigit(prefix))
        {
            octal = true;
            sb.Append('0').Append(_source[_index++]);
        }
        else
        {
            ++_index;
        }

        sb.Append(this.ScanLiteralPart(Character.IsOctalDigitFunc, allowNumericSeparator: !isLegacyOctalDigital));
        var number = sb.ToString();

        if (!octal && number.Length == 0)
        {
            // only 0o or 0O
            TolerateUnexpectedToken();
        }

        var ch = _source.CharCodeAt(_index);
        if (Character.IsIdentifierStart(ch) || Character.IsDecimalDigit(ch))
        {
            TolerateUnexpectedToken();
        }

        double value;

        if (number.Length <= 21) // = Floor(Log8(Pow(2, 64))
        {
            value = Convert.ToUInt64(number.ToString(), fromBase: 8);
        }
        else
        {
            value = ParseIntAsDouble(number.AsSpan(), fromBase: 8);
        }

        return Token.CreateNumericLiteral(value, NumericTokenType.Integer, octal, start, end: _index, _lineNumber, _lineStart);
    }

    private bool IsImplicitOctalLiteral()
    {
        // Implicit octal, unless there is a non-octal digit.
        // (Annex B.1.1 on Numeric Literals)
        for (var i = _index + 1; i < _length; ++i)
        {
            var ch = _source[i];
            if (ch == '8' || ch == '9')
            {
                return false;
            }

            if (!Character.IsOctalDigit(ch))
            {
                return true;
            }
        }

        return true;
    }

    private ReadOnlySpan<char> ScanLiteralPart(Func<char, bool> check, bool allowNumericSeparator)
    {
        var start = _index;

        var charCode = _source.CharCodeAt(_index);
        if (charCode == '_')
        {
            TolerateUnexpectedToken(Messages.NumericSeparatorNotAllowedHere);
        }

        var needsCleanup = false;
        while (check(charCode) || charCode == '_')
        {
            if (charCode == '_')
            {
                if (!allowNumericSeparator)
                {
                    TolerateUnexpectedToken();
                }
                needsCleanup = true;
            }

            _index++;
            var newCharCode = _source.CharCodeAt(_index);
            if (charCode == '_')
            {
                if (newCharCode == '_')
                {
                    TolerateUnexpectedToken(Messages.NumericSeparatorOneUnderscore);
                }
            }

            if (Eof())
            {
                break;
            }
            charCode = newCharCode;
        }

        if (_source[_index - 1] == '_')
        {
            TolerateUnexpectedToken(Messages.NumericSeparatorNotAllowedHere);
        }

        var span = _source.AsSpan(start, _index - start);
        return needsCleanup
            ? span.ToString().Replace("_", "").AsSpan()
            : span;
    }

    private Token ScanNumericLiteral(bool strict)
    {
        var sb = GetStringBuilder();
        var start = _index;
        var ch = _source[start];
        //assert(Character.IsDecimalDigit(ch) || (ch == '.'),
        //    'Numeric literal must start with a decimal digit or a decimal point');

        bool hasComma = false;

        var nonOctal = false;
        string number;
        if (ch != '.')
        {
            var first = _source[_index++];
            ch = _source.CharCodeAt(_index);

            // Hex number starts with '0x'.
            // Octal number starts with '0'.
            // Octal number in ES6 starts with '0o'.
            // Binary number in ES6 starts with '0b'.
            if (first == '0')
            {
                if (ch is 'x' or 'X')
                {
                    ++_index;
                    return ScanHexLiteral(start);
                }

                if (ch is 'b' or 'B')
                {
                    ++_index;
                    return ScanBinaryLiteral(start);
                }

                if (ch is 'o' or 'O')
                {
                    return ScanOctalLiteral(ch, start);
                }

                if (ch is '_')
                {
                    TolerateUnexpectedToken(Messages.NumericSeparatorAfterLeadingZero);
                }

                nonOctal = char.IsNumber(ch);
                if (nonOctal)
                {
                    if (strict)
                    {
                        TolerateUnexpectedToken(Messages.StrictDecimalWithLeadingZero);
                    }
                }

                if (ch > 0 && Character.IsOctalDigit(ch))
                {
                    if (IsImplicitOctalLiteral())
                    {
                        return ScanOctalLiteral(ch, start, true);
                    }
                }
            }

            --_index;
            sb.Append(this.ScanLiteralPart(Character.IsDecimalDigitFunc, allowNumericSeparator: !nonOctal));
            ch = _source.CharCodeAt(_index);
        }

        if (ch == '.')
        {
            sb.Append(_source[_index++]);
            sb.Append(this.ScanLiteralPart(Character.IsDecimalDigitFunc, allowNumericSeparator: !nonOctal));
            ch = _source.CharCodeAt(_index);

            hasComma = true;
        }

        if (ch == 'e' || ch == 'E')
        {
            sb.Append(_source[_index++]);

            ch = _source.CharCodeAt(_index);
            if (ch == '+' || ch == '-')
            {
                sb.Append(_source[_index++]);
            }

            if (Character.IsDecimalDigit(_source.CharCodeAt(_index)))
            {
                sb.Append(this.ScanLiteralPart(Character.IsDecimalDigitFunc, allowNumericSeparator: true));
            }
            else
            {
                TolerateUnexpectedToken();
            }
        }
        else if (ch == 'u' || ch == 'U') // Unsigned
        {
            _index++;

            if (_index < _source.Length && (_source[_index] == 'l' || _source[_index] == 'L')) // Unsigned long
            {
                _index++;
                var ulongValue = ulong.Parse(sb.ToString(), CultureInfo.InvariantCulture);
                return Token.CreateNumericLiteral(ulongValue, NumericTokenType.UnsignedLong, false, start, Index, LineNumber, LineStart);

            }
            else // Unsigned int
            {
                var uintValue = uint.Parse(sb.ToString(), CultureInfo.InvariantCulture);
                return Token.CreateNumericLiteral(uintValue, NumericTokenType.UnsignedInteger, false, start, Index, LineNumber, LineStart);

            }
        }
        else if (ch == 'l' || ch == 'L') // Long
        {
            _index++;

            // Parse as ulong first, since long.MinValue won't be parsed thru long.Parse
            var longValue = ulong.Parse(sb.ToString(), CultureInfo.InvariantCulture);
            if (longValue > (ulong) long.MaxValue + 1)
                TolerateUnexpectedToken("Long literal overflows");

            return Token.CreateNumericLiteral((long) longValue, NumericTokenType.Long, false, start, Index, LineNumber, LineStart);

        }
        else if (ch == 'd' || ch == 'D') // Adhoc: double
        {
            _index++;

            if (double.TryParse(sb.ToString(),
                NumberStyles.AllowDecimalPoint | NumberStyles.AllowExponent | NumberStyles.AllowLeadingSign, CultureInfo.InvariantCulture,
                out var d))
            {
                return Token.CreateNumericLiteral(d, NumericTokenType.Double, false, start, Index, LineNumber, LineStart);
            }
            else
                TolerateUnexpectedToken();
        }

        if (Character.IsIdentifierStart(_source.CharCodeAt(_index)))
        {
            TolerateUnexpectedToken();
        }

        number = sb.ToString();

        if (hasComma)
        {
            if (float.TryParse(number,
                NumberStyles.AllowDecimalPoint | NumberStyles.AllowExponent | NumberStyles.AllowLeadingSign, CultureInfo.InvariantCulture,
                out var f))
            {
                return Token.CreateNumericLiteral(f, NumericTokenType.Float, false, start, Index, LineNumber, LineStart);
            }
            else
            {
                TolerateUnexpectedToken();
                return Token.Create(TokenType.Unknown, null, start, Index, LineNumber, LineStart);
            }
        }
        else
        {

            // Int by default
            if (int.TryParse(
                number,
                NumberStyles.AllowDecimalPoint | NumberStyles.AllowExponent | NumberStyles.AllowLeadingSign,
                CultureInfo.InvariantCulture,
                out var i))
            {
                return Token.CreateNumericLiteral(i, NumericTokenType.Integer, false, start, Index, LineNumber, LineStart);

            }
            else if (long.TryParse(
                number,
                NumberStyles.AllowDecimalPoint | NumberStyles.AllowExponent | NumberStyles.AllowLeadingSign,
                CultureInfo.InvariantCulture,
                out var l))
            {
                return Token.CreateNumericLiteral(l, NumericTokenType.Long, false, start, Index, LineNumber, LineStart);
            }
            else if (float.TryParse(
                number, NumberStyles.AllowDecimalPoint | NumberStyles.AllowExponent | NumberStyles.AllowLeadingSign,
                CultureInfo.InvariantCulture,
                out var f))
            {
                return Token.CreateNumericLiteral(f, NumericTokenType.Float, false, start, Index, LineNumber, LineStart);
            }
            else if (double.TryParse(
                number, NumberStyles.AllowDecimalPoint | NumberStyles.AllowExponent | NumberStyles.AllowLeadingSign,
                CultureInfo.InvariantCulture,
                out var d))
            {
                return Token.CreateNumericLiteral(d, NumericTokenType.Double, false, start, Index, LineNumber, LineStart);
            }
            else
            {
                d = number.TrimStart().StartsWith("-", StringComparison.OrdinalIgnoreCase)
                    ? double.NegativeInfinity
                    : double.PositiveInfinity;

                return Token.CreateNumericLiteral(d, NumericTokenType.Double, false, start, Index, LineNumber, LineStart);

            }
        }
    }

    // https://tc39.github.io/ecma262/#sec-template-literal-lexical-components

    private Token ScanTemplate()
    {
        var sb = GetStringBuilder();
        var terminated = false;
        var start = _index;

        var head = _source[start] == '"';
        var tail = false;
        char notEscapeSequenceHead = default;
        var rawOffset = 2;
        bool hasHexEscape = false;

        ++_index;

        char ch;
        while (!Eof())
        {
            ch = _source[_index++];
            if (ch == '"')
            {
                rawOffset = 1;
                tail = true;
                terminated = true;
                break;
            }
            else if (ch == '%')
            {
                if (_source.CharCodeAt(_index) == '{')
                {
                    _curlyStack.Add("%{");
                    ++_index;
                    terminated = true;
                    break;
                }

                sb.Append(ch);
            }
            else if (notEscapeSequenceHead != default)
            {
                continue;
            }
            else if (ch == '\\')
            {
                if (_index >= _source.Length)
                {
                    break;
                }
                ch = _source[_index++];
                if (!Character.IsLineTerminator(ch))
                {
                    switch (ch)
                    {
                        case 'n':
                            sb.Append('\n');
                            break;
                        case 'r':
                            sb.Append('\r');
                            break;
                        case 't':
                            sb.Append('\t');
                            break;
                        case 'u':
                            if (_source.CharCodeAt(_index) == '{')
                            {
                                ++_index;
                                if (!TryScanUnicodeCodePointEscape(out var cp))
                                {
                                    notEscapeSequenceHead = cp > Character.UnicodeLastCodePoint ? 'v' : 'u';
                                }
                                else
                                {
                                    sb.AppendCodePoint(cp);
                                }
                            }
                            else
                            {
                                if (!ScanHexEscape(ch, out var unescapedChar))
                                {
                                    notEscapeSequenceHead = 'u';
                                }
                                else
                                {
                                    sb.Append(unescapedChar);
                                }
                            }

                            break;
                        case 'x':
                            if (!ScanHexEscape(ch, out var unescaped2))
                            {
                                notEscapeSequenceHead = 'x';
                            }
                            else
                            {
                                sb.Append(unescaped2);
                                hasHexEscape = true;
                            }

                            break;
                        case 'b':
                            sb.Append('\b');
                            break;
                        case 'f':
                            sb.Append('\f');
                            break;
                        case 'v':
                            sb.Append('\v');
                            break;

                        default:
                            if (ch == '0')
                            {
                                if (Character.IsDecimalDigit(_source.CharCodeAt(_index)))
                                {
                                    // NotEscapeSequence: \01 \02 and so on
                                    notEscapeSequenceHead = '0';
                                }
                                else
                                {
                                    sb.Append('\0');
                                }
                            }
                            else if (Character.IsDecimalDigit(ch))
                            {
                                // NotEscapeSequence: \1 \2
                                notEscapeSequenceHead = ch;
                            }
                            else
                            {
                                sb.Append(ch);
                            }

                            break;
                    }
                }
                else
                {
                    ++_lineNumber;
                    if (ch == '\r')
                    {
                        if (_source.CharCodeAt(_index) == '\n')
                        {
                            ++_index;
                        }
                    }

                    _lineStart = _index;
                }
            }
            else if (Character.IsLineTerminator(ch))
            {
                ++_lineNumber;

                if (ch == '\r')
                {
                    if (_source.CharCodeAt(_index) == '\n')
                    {
                        ++_index;
                    }
                    ch = '\n';
                }
                // U+2028 and U+2029 are not normalized to \n.

                _lineStart = _index;
                sb.Append(ch);
            }
            else
            {
                sb.Append(ch);
            }
        }

        if (!terminated)
        {
            TolerateUnexpectedToken();
        }

        if (!head)
        {
            _curlyStack.RemoveAt(_curlyStack.Count - 1);
        }

        var value = notEscapeSequenceHead == default ? sb.ToString() : null;

        var startRaw = start + 1;
        var endRaw = _index - rawOffset;
        var rawTemplate = _source.AsSpan(startRaw, endRaw - startRaw);

        return Token.CreateTemplate(cooked: value, rawTemplate.ToInternedString(ref _stringPool, NonIdentifierInterningThreshold),
            head, tail, notEscapeSequenceHead, hasHexEscape, start, end: _index, _lineNumber, _lineStart);
    }

    public Token ScanIdentifierLiteral()
    {
        var cooked = GetStringBuilder();
        var terminated = false;
        var start = Index;

        ++_index;

        // Note: nil is allowed
        while (!Eof())
        {
            var ch = _source[_index++];
            if (ch == '`')
            {
                terminated = true;
                break;
            }
            else if (Character.IsLineTerminator(ch))
            {
                TolerateUnexpectedToken("Unexpected line terminator in identifier literal");
            }
            else if (ch == '\\')
            {
                ch = _source[_index++];
                if (!Character.IsLineTerminator(ch))
                {
                    if (ch == 'n')
                        TolerateUnexpectedToken("Unexpected line terminator in identifier literal");

                    cooked.Append(ch);
                }
            }
            else
            {
                cooked.Append(ch);
            }
        }

        if (!terminated)
        {
            TolerateUnexpectedToken();
        }

        return Token.Create(TokenType.Identifier, cooked.ToString(), start, Index, LineNumber, LineStart);
    }

    public Token ScanSymbolLiteral()
    {
        var cooked = GetStringBuilder();
        var terminated = false;
        var start = Index;

        ++_index;

        // Note: nil is allowed
        while (!Eof())
        {
            var ch = _source[_index++];
            if (ch == '\'')
            {
                terminated = true;
                break;
            }
            else if (Character.IsLineTerminator(ch))
            {
                TolerateUnexpectedToken("Unexpected line terminator in symbol literal");
            }
            else if (ch == '\\')
            {
                ch = _source[_index++];
                if (!Character.IsLineTerminator(ch))
                {
                    if (ch == 'n')
                        TolerateUnexpectedToken("Unexpected line terminator in symbol literal");

                    cooked.Append(ch);
                }
            }
            else
            {
                cooked.Append(ch);
            }
        }

        if (!terminated)
        {
            TolerateUnexpectedToken();
        }

        return Token.Create(TokenType.SymbolLiteral, cooked.ToString(), start, Index, LineNumber, LineStart);
    }

    public Token Lex() => Lex(new LexOptions());

    internal Token Lex(in LexOptions options)
    {
        if (Eof())
        {
            ReleaseLargeBuffers();

            return Token.CreateEof(_index, _lineNumber, _lineStart);
        }

        var cp = _source[_index];

        // IsIdentifierStart also matches backslash and the surrogate range (U+D800..U+DFFF).
        // Adhoc: Now it doesn't match backslash. :)
        if (Character.IsIdentifierStart(cp))
        {
            return ScanIdentifier(options.AllowIdentifierEscape);
        }

        // Very common: ( and ) and ;
        if (cp == 0x28 || cp == 0x29 || cp == 0x3B)
        {
            return ScanPunctuator();
        }

        // ADHOC: Removed String Literal. We enforce everything to be a template.

        // ADHOC: Identifier literal `
        if (cp == 0x60)
        {
            return ScanIdentifierLiteral();
        }

        // ADHOC: ' - Symbol literal
        if (cp == 0x27)
        {
            return ScanSymbolLiteral();
        }

        // Dot (.) U+002E can also start a floating-point number, hence the need
        // to check the next character.
        if (cp == 0x2E)
        {
            if (Character.IsDecimalDigit(_source.CharCodeAt(_index + 1)))
            {
                return ScanNumericLiteral(options.Strict);
            }

            return ScanPunctuator();
        }

        if (Character.IsDecimalDigit(cp))
        {
            return ScanNumericLiteral(options.Strict);
        }

        // Template literals start with ` (U+0060) for template head
        // or } (U+007D) for template middle or template tail.
        if (cp == 0x22 || cp == 0x7D && _curlyStack.Count > 0 && _curlyStack[^1] == "%{")
        {
            return ScanTemplate();
        }

        return ScanPunctuator();
    }

    internal Marker GetMarker() => new(_index, _lineNumber, Column: _index - _lineStart);
}
