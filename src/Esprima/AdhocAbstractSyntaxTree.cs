using System.Diagnostics.CodeAnalysis;
using System.Globalization;
using System.Runtime.CompilerServices;
using System.Security.Cryptography;
using System.Xml.Linq;
using Esprima.Ast;
using Esprima.Ast.Adhoc;

namespace Esprima;

/// <summary>
/// Provides JavaScript parsing capabilities.
/// </summary>
/// <remarks>
/// Use the <see cref="ParseScript" /> or <see cref="ParseExpression(string, bool)" /> methods to parse the JavaScript code.
/// </remarks>
public partial class AdhocAbstractSyntaxTree
{
    internal sealed class Context
    {
        public Context()
        {
            LabelSet = new HashSet<string?>();
            Reset();
        }

        public void Reset()
        {
            IsModule = false;
            AllowIn = true;
            AllowStrictDirective = true;
            AllowSuperCall = false;
            AllowSuperAccess = false;
            AllowYield = true;
            IsAsync = false;
            FirstCoverInitializedNameError = null;
            IsAssignmentTarget = false;
            IsBindingElement = false;
            InClassFieldInit = false;
            InClassStaticBlock = false;
            InFunctionBody = false;
            InIteration = false;
            InSwitch = false;
            Strict = false;
            AllowIdentifierEscape = false;
            MemberAccessContext = MemberAccessContext.Unknown;

            Decorators.Clear();
            LabelSet.Clear();
        }

        public void ReleaseLargeBuffers()
        {
            Decorators.Clear();
            if (Decorators.Capacity > 64)
            {
                Decorators.Capacity = 64;
            }

            if (LabelSet.Count > 64)
            {
                LabelSet = new HashSet<string?>();
            }
        }

        public bool IsModule;
        public bool AllowIn;
        public bool AllowStrictDirective;
        public bool AllowSuperCall;
        public bool AllowSuperAccess;
        public bool AllowYield;
        public bool IsAsync;
        public bool IsAssignmentTarget;
        public bool IsBindingElement;
        public bool InClassFieldInit;
        public bool InClassStaticBlock;
        public bool InFunctionBody;
        public bool InIteration;
        public bool InSwitch;
        public bool InClassBody;
        public bool Strict;
        public bool AllowIdentifierEscape;
        public MemberAccessContext MemberAccessContext;

        public ArrayList<Decorator> Decorators;

        public HashSet<string?> LabelSet;

        public StrongBox<Token>? FirstCoverInitializedNameError;
    }

    internal enum MemberAccessContext : byte
    {
        Unknown = 0,
        NewExpressionCallee = 1,
        Decorator = 2,
    }

    [StringMatcher("=", "*=", "**=", "/=", "%=", "+=", "-=", "<<=", ">>=", "&=", "^=", "|=", "&&=", "||=", "??=")]
    private static partial bool IsAssignmentOperator(string id);

    // cache frequently called Func so we don't need to build Func<T> instances all the time
    // can be revisited with NET 7 SDK where things have improved
    private readonly Func<Expression> _parseAssignmentExpression;
    private readonly Func<Expression> _parseBinaryExpressionOperand;
    private readonly Func<Expression> _parseUnaryExpression;
    private readonly Func<Expression> _parseExpression;
    private readonly Func<Expression> _parsePrimaryExpression;
    private readonly Func<Expression> _parseGroupExpression;
    private readonly Func<Expression> _parseArrayOrMapInitializer;
    private readonly Func<Expression> _parseBinaryExpression;
    private readonly Func<Expression> _parseLeftHandSideExpression;
    private readonly Func<Expression> _parseLeftHandSideExpressionAllowCall;
    private readonly Func<Statement> _parseStatement;
    private readonly Func<BlockStatement> _parseFunctionSourceElements;
    private readonly Func<Expression> _parseAsyncArgument;

    private protected Token _lookahead;
    private protected readonly Context _context;

    private protected Marker _startMarker;
    private protected Marker _lastMarker;

    private protected readonly ErrorHandler _errorHandler;
    private protected readonly bool _tolerant;
    private protected readonly bool _allowReturnOutsideFunction;
    private protected readonly int _maxAssignmentDepth;
    private readonly Action<Node>? _onNodeCreated;

    private protected readonly Scanner _scanner;
    private protected bool _hasLineTerminator;

    private protected List<SyntaxToken>? _tokens;
    private protected List<SyntaxComment>? _comments;

    /// <summary>
    /// Creates a new <see cref="AdhocAbstractSyntaxTree" /> instance.
    /// </summary>
    public AdhocAbstractSyntaxTree() : this(ParserOptions.Default)
    {
    }

    /// <summary>
    /// Creates a new <see cref="AdhocAbstractSyntaxTree" /> instance.
    /// </summary>
    /// <param name="options">The parser options.</param>
    /// <returns></returns>
    public AdhocAbstractSyntaxTree(ParserOptions options)
    {
        if (options == null)
        {
            throw new ArgumentNullException(nameof(options));
        }

        _errorHandler = options.ErrorHandler;
        _tolerant = options.Tolerant;
        _allowReturnOutsideFunction = options.AllowReturnOutsideFunction;
        _tokens = options.Tokens ? new List<SyntaxToken>() : null;
        _comments = options.Comments ? new List<SyntaxComment>() : null;
        _maxAssignmentDepth = options.MaxAssignmentDepth;
        _onNodeCreated = options.OnNodeCreated;

        _scanner = new Scanner(options.GetScannerOptions());

        _context = new Context();

        _parseAssignmentExpression = ParseAssignmentExpression;
        _parseBinaryExpressionOperand = ParseBinaryExpressionOperand;
        _parseUnaryExpression = ParseUnaryExpression;
        _parseExpression = ParseExpression;
        _parsePrimaryExpression = ParsePrimaryExpression;
        _parseGroupExpression = ParseGroupExpression;
        _parseArrayOrMapInitializer = ParseArrayOrMapInitializer;
        _parseBinaryExpression = ParseBinaryExpression;
        _parseLeftHandSideExpression = ParseLeftHandSideExpression;
        _parseLeftHandSideExpressionAllowCall = ParseLeftHandSideExpressionAllowCall;
        _parseStatement = ParseStatement;
        _parseFunctionSourceElements = ParseFunctionSourceElements;
        _parseAsyncArgument = ParseAsyncArgument;
    }

    private void Reset(string code, string? source)
    {
        _assignmentDepth = 0;
        _hasLineTerminator = false;
        _lookahead = default;

        _markersStack = null;
        _precedencesStack = null;
        _sharedStack = null;
        _parseVariableBindingParameters = null;

        _tokens?.Clear();
        _comments?.Clear();

        _scanner.Reset(code, source);

        _context.Reset();

        _startMarker = new Marker(Index: 0, Line: _scanner._lineNumber, Column: 0);

        NextToken();

        _lastMarker = _scanner.GetMarker();
    }

    private void ReleaseLargeBuffers()
    {
        _scanner.ReleaseLargeBuffers();
        _context.ReleaseLargeBuffers();
    }

    // https://tc39.github.io/ecma262/#sec-scripts

    /// <summary>
    /// Parses the code as a JavaScript script.
    /// </summary>
    public Script ParseScript(string code, string? source = null, bool strict = false)
    {
        Reset(code, source);
        try
        {
            _context.Strict = strict;

            var node = CreateNode();

            // For some reason the script only starts from the first token so startMarker is used to set the line for this.
            // Set it to 1 otherwise #line preprocessor can cause some position/marker asserts.
            node = new Marker(node.Index, 1, node.Column);

            var body = ParseDirectivePrologues();
            while (!IsEndOfFile())
            {
                body.Push(ParseStatementListItem());
            }

            return FinalizeRoot(Finalize(node, new Script(NodeList.From(ref body), _context.Strict)));
        }
        finally
        {
            ReleaseLargeBuffers();
        }
    }

    private protected void CollectComments()
    {
        if (_comments is null)
        {
            _scanner.ScanCommentsInternal();
        }
        else
        {
            foreach (var e in _scanner.ScanCommentsInternal().AsReadOnlySpan())
            {
                var value = _scanner._source.AsSpan(e.Slice.Start, e.Slice.Length)
                    .ToInternedString(ref _scanner._stringPool, Scanner.NonIdentifierInterningThreshold);

                var comment = new SyntaxComment(e.Type, value)
                {
                    Range = new Range(e.Start, e.End),
                    Location = new Location(e.StartPosition, e.EndPosition, _scanner._sourceLocation)
                };

                _comments.Add(comment);
            }
        }
    }

    /// <summary>
    /// From internal representation to an external structure
    /// </summary>
    private protected string GetTokenRaw(in Token token)
    {
        switch (token.Type)
        {
            // In the following cases token.Value is already a single-character cached string or interned string.
            // (See Scanner.ScanIdentifier and Scanner.ScanPunctuator)
            case TokenType.Punctuator:
                return (string) token.Value!;

            case TokenType.Identifier:
            case TokenType.Keyword:
            case TokenType.NilLiteral:
            case TokenType.BooleanLiteral:
                var stringValue = (string) token.Value!;
                // Identifiers may contain escaped characters.
                // (In tolerant mode even identifiers like "nul\u{6c}" may be accepted as keywords, null or boolean literals.)
                if (!token.IsEscaped(stringValue))
                {
                    return stringValue;
                }
                break;

            // In these cases we want to intern short literals only.
            case TokenType.StringLiteral:
            case TokenType.Template:
                return _scanner._source.Between(token.Start, token.End)
                    .ToInternedString(ref _scanner._stringPool, Scanner.NonIdentifierInterningThreshold);
        }

        return _scanner._source.Between(token.Start, token.End).ToInternedString(ref _scanner._stringPool);
    }

    private protected SyntaxToken FinalizeToken(int start, int end, SyntaxToken token)
    {
        var startPosition = new Position(_startMarker.Line, _startMarker.Column);
        var endPosition = new Position(_scanner._lineNumber, _scanner._index - _scanner._lineStart);

        token.Range = new Range(start, end);
        token.Location = new Location(startPosition, endPosition);

        return token;
    }

    private protected SyntaxToken ConvertToken(in Token token)
    {
        return FinalizeToken(token.Start, token.End, new SyntaxToken(token.Type, GetTokenRaw(token)));
    }

    private protected Token NextToken(bool allowIdentifierEscape = false)
    {
        var token = _lookahead;

        _lastMarker = _scanner.GetMarker();

        CollectComments();

        if (_scanner._index != _startMarker.Index)
        {
            _startMarker = _scanner.GetMarker();
        }

        var next = _scanner.Lex(new LexOptions(_context.Strict, allowIdentifierEscape));
        _hasLineTerminator = token.Type != TokenType.Unknown && next.Type != TokenType.Unknown && token.LineNumber != next.LineNumber;

        _lookahead = next;

        if (_tokens is not null && next.Type != TokenType.Unknown && next.Type != TokenType.EOF)
        {
            _tokens.Add(ConvertToken(next));
        }

        return token;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private Marker CreateNode()
    {
        return new Marker(_startMarker.Index, _startMarker.Line, _startMarker.Column);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static Marker StartNode(in Token token, int lastLineStart = 0)
    {
        var column = token.Start - token.LineStart;
        var line = token.LineNumber;
        if (column < 0)
        {
            column += lastLineStart;
            line--;
        }

        return new Marker(token.Start, line, column);
    }

    private protected T Finalize<T>(in Marker marker, T node) where T : Node
    {
        node.Range = new Range(marker.Index, _lastMarker.Index);

        var start = new Position(marker.Line, marker.Column);
        var end = new Position(_lastMarker.Line, _lastMarker.Column);

        node.Location = new Location(start, end, _scanner._sourceLocation);

        _onNodeCreated?.Invoke(node);

        return node;
    }

    private T FinalizeRoot<T>(T node) where T : Node, ISyntaxTreeRoot
    {
        if (_tokens is not null)
        {
            _tokens.TrimExcess();
            node.Tokens = _tokens;
            _tokens = new List<SyntaxToken>();
        }

        if (_comments is not null)
        {
            _comments.TrimExcess();
            node.Comments = _comments;
            _comments = new List<SyntaxComment>();
        }

        return node;
    }

    /// <summary>
    /// Expect the next token to match the specified punctuator.
    /// If not, an exception will be thrown.
    /// </summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void Expect(string value)
    {
        var token = NextToken(allowIdentifierEscape: true);
        if (token.Type != TokenType.Punctuator || !value.Equals((string) token.Value!, StringComparison.Ordinal))
        {
            TolerateUnexpectedToken(token);
        }
    }

    /// <summary>
    /// Quietly expect a comma when in tolerant mode, otherwise delegates to Expect().
    /// </summary>
    private void ExpectCommaSeparator()
    {
        if (_tolerant)
        {
            var token = _lookahead;
            if (token.Type == TokenType.Punctuator && ",".Equals(token.Value))
            {
                NextToken();
            }
            else if (token.Type == TokenType.Punctuator && ";".Equals(token.Value))
            {
                NextToken();
                TolerateUnexpectedToken(token);
            }
            else
            {
                TolerateUnexpectedToken(token, Messages.UnexpectedToken);
            }
        }
        else
        {
            Expect(",");
        }
    }

    /// <summary>
    /// Expect the next token to match the specified keyword.
    /// If not, an exception will be thrown.
    /// </summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void ExpectKeyword(string keyword)
    {
        var token = NextToken();
        if (token.Type != TokenType.Keyword || !keyword.Equals((string) token.Value!, StringComparison.Ordinal))
        {
            TolerateUnexpectedToken(token);
        }
    }

    /// <summary>
    /// Expect the next token to match the specified keyword.
    /// If not, an exception will be thrown.
    /// </summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void ExpectKeyword(params string[] keyword)
    {
        var token = NextToken();
        foreach (var word in keyword)
        {
            if (token.Type == TokenType.Keyword && word.Equals(token.Value))
                continue;
        }
    }

    /// <summary>
    /// Return true if the next token matches the specified punctuator.
    /// </summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private protected bool Match(string value)
    {
        // ReSharper disable once InlineTemporaryVariable
        ref readonly var token = ref _lookahead;
        return token.Type == TokenType.Punctuator && value.Equals((string) token.Value!, StringComparison.Ordinal);
    }

    /// <summary>
    /// Return true if the next token matches the specified punctuator and consumes the next token.
    /// </summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private protected bool ConsumeMatch(string value)
    {
        if (Match(value))
        {
            NextToken();
            return true;
        }

        return false;
    }

    /// <summary>
    /// Return true if the next token matches any of the specified punctuators.
    /// </summary>
    private bool MatchAny(char value1, char value2, char value3, char value4)
    {
        // ReSharper disable once InlineTemporaryVariable
        ref readonly var token = ref _lookahead;
        if (token.Type != TokenType.Punctuator || token.End - token.Start != 1)
        {
            return false;
        }

        var c = ((string) token.Value!)[0];
        return c == value1 || c == value2 || c == value3 || c == value4;
    }

    /// <summary>
    /// Return true if the next token matches the specified keyword
    /// </summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private bool MatchKeyword(string keyword)
    {
        // ReSharper disable once InlineTemporaryVariable
        ref readonly var token = ref _lookahead;
        return token.Type == TokenType.Keyword && keyword.Equals((string) token.Value!, StringComparison.Ordinal);
    }

    // Return true if the next token matches the specified contextual keyword
    // (where an identifier is sometimes a keyword depending on the context)

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private bool MatchContextualKeyword(string keyword)
    {
        // ReSharper disable once InlineTemporaryVariable
        ref readonly var token = ref _lookahead;
        return token.Type == TokenType.Identifier && keyword.Equals((string) token.Value!, StringComparison.Ordinal);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private bool IsEndOfFile()
    {
        return _lookahead.Type == TokenType.EOF;
    }

    // Return true if the next token is an assignment operator

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private bool MatchAssign()
    {
        // ReSharper disable once InlineTemporaryVariable
        ref readonly var token = ref _lookahead;
        return token.Type == TokenType.Punctuator && IsAssignmentOperator((string) token.Value!);
    }

    // Cover grammar support.
    //
    // When an assignment expression position starts with an left parenthesis, the determination of the type
    // of the syntax is to be deferred arbitrarily long until the end of the parentheses pair (plus a lookahead)
    // or the first comma. This situation also defers the determination of all the expressions nested in the pair.
    //
    // There are three productions that can be parsed in a parentheses pair that needs to be determined
    // after the outermost pair is closed. They are:
    //
    //   1. AssignmentExpression
    //   2. BindingElements
    //   3. AssignmentTargets
    //
    // In order to avoid exponential backtracking, we use two flags to denote if the production can be
    // binding element or assignment target.
    //
    // The three productions have the relationship:
    //
    //   BindingElements ⊆ AssignmentTargets ⊆ AssignmentExpression
    //
    // with a single exception that CoverInitializedName when used directly in an Expression, generates
    // an early error. Therefore, we need the third state, firstCoverInitializedNameError, to track the
    // first usage of CoverInitializedName and report it when we reached the end of the parentheses pair.
    //
    // isolateCoverGrammar function runs the given parser function with a new cover grammar context, and it does not
    // effect the current flags. This means the production the parser parses is only used as an expression. Therefore
    // the CoverInitializedName check is conducted.
    //
    // inheritCoverGrammar function runs the given parse function with a new cover grammar context, and it propagates
    // the flags outside of the parser. This means the production the parser parses is used as a part of a potential
    // pattern. The CoverInitializedName check is deferred.

    private T IsolateCoverGrammar<T>(Func<T> parseFunction) where T : Node
    {
        var previousIsBindingElement = _context.IsBindingElement;
        var previousIsAssignmentTarget = _context.IsAssignmentTarget;
        var previousFirstCoverInitializedNameError = _context.FirstCoverInitializedNameError;

        _context.IsBindingElement = true;
        _context.IsAssignmentTarget = true;
        _context.FirstCoverInitializedNameError = null;

        var result = parseFunction();
        if (_context.FirstCoverInitializedNameError is not null)
        {
            TolerateUnexpectedToken(_context.FirstCoverInitializedNameError.Value);
        }

        _context.IsBindingElement = previousIsBindingElement;
        _context.IsAssignmentTarget = previousIsAssignmentTarget;
        _context.FirstCoverInitializedNameError = previousFirstCoverInitializedNameError;

        return result;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining | (MethodImplOptions) 512)]
    private T InheritCoverGrammar<T>(Func<T> parseFunction) where T : Node
    {
        var previousIsBindingElement = _context.IsBindingElement;
        var previousIsAssignmentTarget = _context.IsAssignmentTarget;
        var previousFirstCoverInitializedNameError = _context.FirstCoverInitializedNameError;

        _context.IsBindingElement = true;
        _context.IsAssignmentTarget = true;
        _context.FirstCoverInitializedNameError = null;

        var result = parseFunction();

        _context.IsBindingElement = _context.IsBindingElement && previousIsBindingElement;
        _context.IsAssignmentTarget = _context.IsAssignmentTarget && previousIsAssignmentTarget;
        _context.FirstCoverInitializedNameError = previousFirstCoverInitializedNameError ?? _context.FirstCoverInitializedNameError;

        return result;
    }

    private void ConsumeSemicolon()
    {
        if (Match(";"))
        {
            NextToken(allowIdentifierEscape: !_context.Strict);
        }
        else if (!_hasLineTerminator)
        {
            if (_lookahead.Type != TokenType.EOF && !Match("}"))
            {
                TolerateUnexpectedToken(_lookahead);
            }

            _lastMarker = _startMarker;
        }
    }

    // https://tc39.github.io/ecma262/#sec-primary-expression

    [MethodImpl((MethodImplOptions) 512)]
    private protected virtual Expression ParsePrimaryExpression()
    {
        var node = CreateNode();

        Expression expr;
        Token token;
        string raw;

        switch (_lookahead.Type)
        {
            case TokenType.Identifier:
                if (_context.IsAsync && "await".Equals(_lookahead.Value))
                {
                    TolerateUnexpectedToken(_lookahead);
                }

                //token = NextToken();
                expr = MatchAsyncFunction() ? ParseFunctionExpression() : Finalize(node, new Identifier((string) NextToken().Value!));
                break;

            case TokenType.StringLiteral:
                _context.IsAssignmentTarget = false;
                _context.IsBindingElement = false;
                token = NextToken();
                raw = GetTokenRaw(token);
                expr = Finalize(node, new Literal(TokenType.StringLiteral, token.Value, raw));
                break;

            case TokenType.NumericLiteral:
                _context.IsAssignmentTarget = false;
                _context.IsBindingElement = false;
                token = NextToken();
                raw = GetTokenRaw(token);
                expr = Finalize(node, new Literal(token.NumericTokenType, token.Value!, raw));
                break;

            case TokenType.BooleanLiteral:
                _context.IsAssignmentTarget = false;
                _context.IsBindingElement = false;
                token = NextToken();
                raw = GetTokenRaw(token);
                expr = Finalize(node, new Literal("true".Equals(token.Value), raw));
                break;

            case TokenType.NilLiteral:
                _context.IsAssignmentTarget = false;
                _context.IsBindingElement = false;
                token = NextToken();
                raw = GetTokenRaw(token);
                expr = Finalize(node, new Literal(raw));
                break;

            case TokenType.Template:
                expr = ParseTemplateLiteral(false);
                break;

            case TokenType.SymbolLiteral:
                if (_context.Strict && _lookahead.Octal)
                {
                    TolerateUnexpectedToken(_lookahead, Messages.StrictOctalLiteral);
                }

                _context.IsAssignmentTarget = false;
                _context.IsBindingElement = false;
                token = NextToken();
                raw = GetTokenRaw(token);

                expr = Finalize(node, new Literal(TokenType.SymbolLiteral, (string?) token.Value, raw));
                break;

            case TokenType.Punctuator:
                switch ((string?) _lookahead.Value)
                {
                    case "|": // ADHOC
                        expr = ParseListAssignmentElementList();
                        break;
                    case "(":
                        _context.IsBindingElement = false;
                        expr = InheritCoverGrammar(_parseGroupExpression);
                        break;
                    case "[":
                        expr = InheritCoverGrammar(_parseArrayOrMapInitializer);
                        break;
                    default:
                        token = NextToken();
                        TolerateUnexpectedToken(token);
                        expr = new ErrorExpression();
                        break;
                }

                break;
            case TokenType.Keyword:

                if (!_context.Strict && _context.AllowYield && MatchKeyword("yield"))
                {
                    expr = ParseIdentifierName();
                }
                else
                {
                    _context.IsAssignmentTarget = false;
                    _context.IsBindingElement = false;

                    if (MatchKeyword("function"))
                    {
                        expr = ParseFunctionExpression();
                    }
                    else if (MatchKeyword("method"))
                    {
                        expr = ParseMethodExpression();
                    }
                    else if (MatchKeyword("self"))
                    {
                        NextToken();

                        if (MatchKeyword("finally"))
                        {
                            expr = ParseSelfFinalizer();
                        }
                        else
                        {
                            expr = Finalize(node, new SelfExpression());
                        }
                    }
                    else if (MatchKeyword("yield"))
                    {
                        expr = ParseYieldExpression();
                    }
                    /* ADHOC: Not available in adhoc
                    else if (MatchKeyword("new"))
                    {
                        expr = ParseNewExpression();
                    }
                    */
                    else if (MatchKeyword("import")) // ADHOC
                    {
                        var decl = ParseImportDeclaration();
                        expr = Finalize(node, new ImportExpression(decl)); // Hack hack hack
                    }
                    else
                    {
                        TolerateUnexpectedToken(_lookahead);
                        NextToken();

                        return Finalize(node, new ErrorExpression());
                    }
                }

                break;
            default:
                token = NextToken();
                TolerateUnexpectedToken(token);
                expr = new ErrorExpression();

                break;
        }

        return expr;
    }

    private Expression ParseGroupExpression()
    {
        Expression expr;

        Expect("(");

        var startToken = _lookahead;

        _context.IsBindingElement = true;
        expr = InheritCoverGrammar(_parseAssignmentExpression);

        if (Match(","))
        {
            var expressions = new ArrayList<Expression>();

            _context.IsAssignmentTarget = false;
            expressions.Add(expr);
            while (true)
            {
                if (IsEndOfFile())
                {
                    TolerateUnexpectedToken(_lookahead);
                    break;
                }

                if (!Match(","))
                {
                    break;
                }

                NextToken();
                expressions.Add(InheritCoverGrammar(_parseAssignmentExpression));
            }

            expr = Finalize(StartNode(startToken), new SequenceExpression(NodeList.From(ref expressions)));
        }

        Expect(")");

        _context.IsBindingElement = false;

        return expr;
    }

    // https://tc39.es/proposal-template-literal-revision/#sec-static-semantics-template-early-errors
    private void ThrowTemplateLiteralEarlyErrors(in Token token)
    {
        switch (token.NotEscapeSequenceHead)
        {
            case 'u':
                TolerateUnexpectedToken(token, Messages.InvalidUnicodeEscapeSequence);
                break;
            case 'v':
                TolerateUnexpectedToken(token, Messages.UndefinedUnicodeCodePoint);
                break;
            case 'x':
                TolerateUnexpectedToken(token, Messages.InvalidHexEscapeSequence);
                break;
            case '8':
            case '9':
                TolerateUnexpectedToken(token, Messages.TemplateEscape89);
                break;
            default: // For 0-7
                TolerateUnexpectedToken(token, Messages.TemplateOctalLiteral);
                break;
        }
    }

    // https://tc39.github.io/ecma262/#sec-array-initializer

    private SpreadElement ParseSpreadElement()
    {
        var node = CreateNode();
        Expect("...");
        var arg = InheritCoverGrammar(_parseAssignmentExpression);
        return Finalize(node, new SpreadElement(arg));
    }

    // https://tc39.github.io/ecma262/#sec-object-initializer

    private BlockStatement ParsePropertyMethod(ref ParsedParameters parameters, out bool hasStrictDirective)
    {
        _context.IsAssignmentTarget = false;
        _context.IsBindingElement = false;

        var previousInClassStaticBlock = _context.InClassStaticBlock;
        var previousStrict = _context.Strict;
        var previousAllowStrictDirective = _context.AllowStrictDirective;
        _context.InClassStaticBlock = false;
        _context.AllowStrictDirective = parameters.Simple;
        var body = IsolateCoverGrammar(_parseFunctionSourceElements);
        hasStrictDirective = _context.Strict;
        if (_context.Strict && parameters.FirstRestricted != null)
        {
            TolerateUnexpectedToken(parameters.FirstRestricted.Value, parameters.Message);
        }

        if (_context.Strict && parameters.Stricted != null)
        {
            TolerateUnexpectedToken(parameters.Stricted.Value, parameters.Message);
        }

        _context.Strict = previousStrict;
        _context.AllowStrictDirective = previousAllowStrictDirective;
        _context.InClassStaticBlock = previousInClassStaticBlock;

        return body;
    }

    // https://tc39.github.io/ecma262/#sec-template-literals

    private TemplateElement ParseTemplateHead(bool isTagged)
    {
        //assert(_lookahead.head, 'Template literal must start with a template head');

        var node = CreateNode();
        var token = NextToken();
        if (!isTagged && token.NotEscapeSequenceHead != default)
        {
            ThrowTemplateLiteralEarlyErrors(token);
        }

        var value = new TemplateElement.TemplateElementValue(Raw: token.RawTemplate!, Cooked: (string) token.Value!, HasHexEscape: token.HasHexEscape);

        return Finalize(node, new TemplateElement(value, token.Tail));
    }

    private TemplateElement ParseTemplateElement(bool isTagged)
    {
        if (_lookahead.Type != TokenType.Template)
        {
            TolerateUnexpectedToken(_lookahead);
        }

        var node = CreateNode();
        var token = NextToken();
        if (!isTagged && token.NotEscapeSequenceHead != default)
        {
            ThrowTemplateLiteralEarlyErrors(token);
        }

        var value = new TemplateElement.TemplateElementValue(Raw: token.RawTemplate!, Cooked: (string) token.Value!, HasHexEscape: token.HasHexEscape);

        return Finalize(node, new TemplateElement(value, token.Tail));
    }

    private TemplateLiteral ParseTemplateLiteral(bool isTagged)
    {
        var node = CreateNode();

        var expressions = new ArrayList<Expression>();
        var quasis = new ArrayList<TemplateElement>();

        var quasi = ParseTemplateHead(isTagged);
        if (!string.IsNullOrEmpty(quasi.Value.Cooked) || quasi.Tail)
            quasis.Add(quasi);

        while (!quasi.Tail)
        {
            if (IsEndOfFile())
            {
                TolerateUnexpectedToken(_lookahead);
                break;
            }

            expressions.Add(ParseExpression());
            quasi = ParseTemplateElement(isTagged);
            if (!string.IsNullOrEmpty(quasi.Value.Cooked))
                quasis.Add(quasi);
        }

        return Finalize(node, new TemplateLiteral(NodeList.From(ref quasis), NodeList.From(ref expressions)));
    }

    // https://tc39.github.io/ecma262/#sec-grouping-operator

    private static Expression ReinterpretExpressionAsPattern(Expression expr)
    {
        // In esprima this method mutates the expression that is passed as a parameter.
        // Because the type property is mutated we need to change the behavior to cloning
        // it instead. As a matter of fact the callers need to replace the actual value that
        // was sent by the returned one.

        Expression node = expr;

        switch (expr.Type)
        {
            case Nodes.Identifier:
            case Nodes.MemberExpression:
            case Nodes.RestElement:
            case Nodes.AssignmentPattern:
                break;
            case Nodes.SpreadElement:
                var newArgument = ReinterpretExpressionAsPattern(expr.As<SpreadElement>().Argument);
                node = new RestElement(newArgument);
                node.Range = expr.Range;
                node.Location = expr.Location;
                break;
            case Nodes.ArrayExpression:
                var elements = new ArrayList<Node?>();

                foreach (var element in expr.As<ArrayExpression>().Elements)
                {
                    if (element != null)
                    {
                        elements.Add(ReinterpretExpressionAsPattern(element));
                    }
                    else
                    {
                        // Add the 'null' value
                        elements.Add(null);
                    }
                }

                node = new ArrayPattern(NodeList.From(ref elements));
                node.Range = expr.Range;
                node.Location = expr.Location;

                break;

            case Nodes.AssignmentExpression:
                var assignmentExpression = expr.As<AssignmentExpression>();
                node = new AssignmentPattern(assignmentExpression.Left, assignmentExpression.Right);
                node.Range = expr.Range;
                node.Location = expr.Location;

                break;
            default:
                // Allow other node type for tolerant parsing.
                break;
        }

        return node;
    }

    // https://tc39.github.io/ecma262/#sec-left-hand-side-expressions

    private NodeList<Expression> ParseArguments()
    {
        var args = new ArrayList<Expression>();

        Expect("(");

        if (!Match(")"))
        {
            while (true)
            {
                if (IsEndOfFile())
                {
                    TolerateUnexpectedToken(_lookahead);
                    break;
                }

                var expr = Match("...")
                    ? ParseSpreadElement()
                    : IsolateCoverGrammar(_parseAssignmentExpression);

                args.Add(expr);
                if (Match(")"))
                {
                    break;
                }

                ExpectCommaSeparator();
                if (Match(")"))
                {
                    break;
                }
            }
        }

        Expect(")");

        return NodeList.From(ref args);
    }

    private static bool IsIdentifierName(in Token token)
    {
        return token.Type == TokenType.Identifier ||
               token.Type == TokenType.Keyword ||
               token.Type == TokenType.BooleanLiteral ||
               token.Type == TokenType.NilLiteral;
    }

    private Identifier ParseIdentifierName()
    {
        var node = CreateNode();

        var token = NextToken();

        if (!IsIdentifierName(token))
        {
            TolerateUnexpectedToken(token);
        }

        return Finalize(node, new Identifier((string) token.Value!));
    }

    private Expression ParseAsyncArgument()
    {
        var arg = ParseAssignmentExpression();
        _context.FirstCoverInitializedNameError = null;
        return arg;
    }

    private NodeList<Expression> ParseAsyncArguments()
    {
        Expect("(");
        var args = new ArrayList<Expression>();

        if (!Match(")"))
        {
            while (true)
            {
                if (IsEndOfFile())
                {
                    TolerateUnexpectedToken(_lookahead);
                    break;
                }

                var expr = Match("...") ? ParseSpreadElement() : IsolateCoverGrammar(_parseAsyncArgument);
                args.Add(expr);
                if (Match(")"))
                {
                    break;
                }

                ExpectCommaSeparator();
                if (Match(")"))
                {
                    break;
                }
            }
        }

        Expect(")");

        return NodeList.From(ref args);
    }

    private Expression ParseLeftHandSideExpressionAllowCall()
    {
        var startMarker = StartNode(_lookahead);
        var maybeAsync = MatchContextualKeyword("async");

        var previousAllowIn = _context.AllowIn;
        _context.AllowIn = true;

        bool isTopLevelScopeResolution = Match("::");
        var node = CreateNode();
        if (isTopLevelScopeResolution)
            NextToken();

        Expression expr = InheritCoverGrammar(_parsePrimaryExpression);
        if (isTopLevelScopeResolution)
        {
            if (expr.Type == Nodes.Identifier)
            {
                if (!Match("::")) // Pointless if next is scope navigation
                {
                    expr = Finalize(node, new StaticIdentifier((Identifier) expr));
                }
            }
            else
            {
                TolerateError("Scope resolution operator prefix is only applicable to identifiers");
            }
        }

        var hasOptional = false;
        while (true)
        {
            if (IsEndOfFile())
            {
                TolerateUnexpectedToken(_lookahead);
                break;
            }

            var optional = false;
            var isComputedOptional = false;
            if (Match("?."))
            {
                if (_context.MemberAccessContext == MemberAccessContext.Decorator)
                {
                    TolerateError(Messages.InvalidDecoratorMemberExpression);
                }

                optional = true;
                hasOptional = true;
                Expect("?.");
            }

            if (Match("?["))
            {
                optional = true;
                hasOptional = true;
                isComputedOptional = true;
                Expect("?[");
            }

            if (Match("("))
            {
                expr = ParseCallExpression(maybeAsync, startMarker, expr, optional);
            }
            else if (Match(".*"))
            {
                NextToken();

                _context.IsBindingElement = false;
                _context.IsAssignmentTarget = !optional;

                var property = IsolateCoverGrammar(_parseLeftHandSideExpressionAllowCall);
                expr = Finalize(startMarker, new ObjectSelectorMemberExpression(expr, property, optional));
            }
            else if (Match("["))
            {
                if (_context.MemberAccessContext == MemberAccessContext.Decorator)
                {
                    TolerateError(Messages.InvalidDecoratorMemberExpression);
                    break;
                }

                _context.IsBindingElement = false;
                _context.IsAssignmentTarget = !optional;
                Expect("[");
                var property = IsolateCoverGrammar(_parseExpression);
                Expect("]");
                expr = Finalize(startMarker, new ComputedMemberExpression(expr, property, optional));
            }
            else if (isComputedOptional)
            {
                _context.IsBindingElement = false;
                _context.IsAssignmentTarget = !optional;

                var property = IsolateCoverGrammar(_parseExpression);
                Expect("]");
                expr = Finalize(startMarker, new ComputedMemberExpression(expr, property, optional));
            }
            else if (_lookahead.Type == TokenType.Template && _lookahead.Head)
            {
                // Optional template literal is not included in the spec.
                // https://github.com/tc39/proposal-optional-chaining/issues/54
                if (optional)
                {
                    TolerateUnexpectedToken(_lookahead);
                }

                if (hasOptional)
                {
                    TolerateError(Messages.InvalidTaggedTemplateOnOptionalChain);
                    break;
                }

                var quasi = ParseTemplateLiteral(true);
                expr = Finalize(startMarker, new TaggedTemplateExpression(expr, quasi));
            }
            else if (Match(".") || optional)
            {
                _context.IsBindingElement = false;
                _context.IsAssignmentTarget = !optional;
                if (!optional)
                {
                    Expect(".");
                }

                Expression property = ParseIdentifierName();

                while (Match("::"))
                {
                    if (IsEndOfFile())
                    {
                        TolerateUnexpectedToken(_lookahead);
                        break;
                    }

                    NextToken();
                    var property2 = ParseIdentifierName();

                    property = Finalize(startMarker, new StaticMemberExpression(property, property2, optional)); // TODO: Check if this is valid (marker)
                }

                expr = Finalize(startMarker, new AttributeMemberExpression(expr, property, optional));
            }
            else if (Match("::") || optional)
            {
                _context.IsBindingElement = false;
                _context.IsAssignmentTarget = !optional;
                if (!optional)
                {
                    Expect("::");
                }

                var property = ParseIdentifierName();
                expr = Finalize(startMarker, new StaticMemberExpression(expr, property, optional));
            }
            else
            {
                break;
            }
        }

        _context.AllowIn = previousAllowIn;

        if (hasOptional)
        {
            return Finalize(startMarker, new ChainExpression(expr));
        }

        return expr;
    }

    private Expression ParseCallExpression(bool maybeAsync, in Marker startToken, Expression callee, bool optional)
    {
        var asyncArrow = maybeAsync && startToken.Line == _lookahead.LineNumber;
        _context.IsBindingElement = false;
        _context.IsAssignmentTarget = false;
        var args = asyncArrow ? ParseAsyncArguments() : ParseArguments();

        Expression expr = Finalize(startToken, new CallExpression(callee, args, optional));
        return expr;
    }

    private Expression ParseLeftHandSideExpression()
    {
        //assert(_context.AllowIn, 'callee of new expression always allow in keyword.');

        var startMarker = StartNode(_lookahead);
        var expr = InheritCoverGrammar(_parsePrimaryExpression);

        var hasOptional = false;
        while (true)
        {
            if (IsEndOfFile())
            {
                TolerateUnexpectedToken(_lookahead);
                break;
            }

            var optional = false;
            if (Match("?."))
            {
                optional = true;
                hasOptional = true;
                Expect("?.");
            }

            if (Match("["))
            {
                _context.IsBindingElement = false;
                _context.IsAssignmentTarget = !optional;
                Expect("[");
                var property = IsolateCoverGrammar(_parseExpression);
                Expect("]");
                expr = Finalize(startMarker, new ComputedMemberExpression(expr, property, optional));
            }
            else if (_lookahead.Type == TokenType.Template && _lookahead.Head)
            {
                // Optional template literal is not included in the spec.
                // https://github.com/tc39/proposal-optional-chaining/issues/54
                if (optional)
                {
                    TolerateUnexpectedToken(_lookahead);
                }

                if (hasOptional)
                {
                    TolerateError(Messages.InvalidTaggedTemplateOnOptionalChain);
                    break;
                }

                var quasi = ParseTemplateLiteral(true);
                expr = Finalize(startMarker, new TaggedTemplateExpression(expr, quasi));
            }
            else if (Match(".") || optional)
            {
                _context.IsBindingElement = false;
                _context.IsAssignmentTarget = !optional;
                if (!optional)
                {
                    Expect(".");
                }

                var property = ParseIdentifierName();

                expr = Finalize(startMarker, new AttributeMemberExpression(expr, property, optional));
            }
            else if (Match("::") || optional) // ADHOC: Static
            {
                _context.IsBindingElement = false;
                _context.IsAssignmentTarget = !optional;
                if (!optional)
                {
                    Expect("::");
                }

                var property = ParseIdentifierName();
                expr = Finalize(startMarker, new StaticMemberExpression(expr, property, optional));
            }
            else
            {
                break;
            }
        }

        if (hasOptional)
        {
            return Finalize(startMarker, new ChainExpression(expr));
        }

        return expr;
    }

    // https://tc39.github.io/ecma262/#sec-update-expressions

    private Expression ParseUpdateExpression()
    {
        Expression expr;
        var startMarker = StartNode(_lookahead);

        if (Match("++") || Match("--"))
        {
            expr = ParsePrefixUnaryExpression(startMarker);
        }
        else
        {
            var previousMemberAccessContext = _context.MemberAccessContext;
            _context.MemberAccessContext = MemberAccessContext.Unknown;
            expr = InheritCoverGrammar(_parseLeftHandSideExpressionAllowCall);
            _context.MemberAccessContext = previousMemberAccessContext;

            if (!_hasLineTerminator && _lookahead.Type == TokenType.Punctuator && (Match("++") || Match("--")))
            {
                expr = ParsePostfixUnaryExpression(expr, startMarker);
            }
        }

        return expr;
    }

    private Expression ParsePostfixUnaryExpression(Expression expr, in Marker marker)
    {
        if (!_context.IsAssignmentTarget)
        {
            TolerateError(Messages.InvalidLHSInAssignment);
        }

        _context.IsAssignmentTarget = false;
        _context.IsBindingElement = false;
        var op = NextToken().Value;
        expr = Finalize(marker, new UpdateExpression((string) op!, expr, prefix: false));
        return expr;
    }

    private Expression ParsePrefixUnaryExpression(in Marker marker)
    {
        var token = NextToken();
        var expr = InheritCoverGrammar(_parseUnaryExpression);

        if (!_context.IsAssignmentTarget)
        {
            TolerateError(Messages.InvalidLHSInAssignment);
        }

        expr = Finalize(marker, new UpdateExpression((string) token.Value!, expr, prefix: true));
        _context.IsAssignmentTarget = false;
        _context.IsBindingElement = false;
        return expr;
    }

    // https://tc39.github.io/ecma262/#sec-unary-operators
    private AwaitExpression ParseAwaitExpression()
    {
        var node = CreateNode();
        NextToken();
        var argument = ParseUnaryExpression();
        return Finalize(node, new AwaitExpression(argument));
    }

    private Expression ParseUnaryExpression()
    {
        Expression expr;
        if (MatchAny('+', '-', '~', '!') || MatchAny('*', '&', '*', '&')) // ADHOC: Added * and &
        {
            expr = ParseBasicUnaryExpression();
        }
        else if (_context.IsAsync && MatchContextualKeyword("await"))
        {
            if (_lookahead.IsEscaped("await"))
            {
                TolerateUnexpectedToken(_lookahead, Messages.InvalidEscapedReservedWord);
            }

            expr = ParseAwaitExpression();
        }
        else
        {
            expr = ParseUpdateExpression();
        }

        return expr;
    }

    private UnaryExpression ParseBasicUnaryExpression()
    {
        bool canAssign = Match("*") || Match("&");

        var startMarker = StartNode(_lookahead);
        var token = NextToken();
        var expr = InheritCoverGrammar(_parseUnaryExpression);
        var unaryExpr = Finalize(startMarker, new UnaryExpression((string) token.Value!, expr));

        if (!canAssign)
        {
            _context.IsAssignmentTarget = false;
            _context.IsBindingElement = false;
        }
        return unaryExpr;
    }

    private static BinaryExpression CreateBinaryExpression(string op, Expression left, Expression right)
    {
        switch (op)
        {
            case "&&":
            case "||":
            case "??":
                return new LogicalExpression(op, left, right);
            default:
                return new BinaryExpression(op, left, right);
        }
    }

    private Expression ParseBinaryExpressionOperand()
    {
        var startMarker = StartNode(_lookahead);

        var isLeftParenthesized = this.Match("(");
        var expr = InheritCoverGrammar(_parseUnaryExpression);

        var exponentAllowed = expr.Type != Nodes.UnaryExpression || isLeftParenthesized;

        if (exponentAllowed && ConsumeMatch("**"))
        {
            expr = ParseExponentiationExpression(expr, startMarker);
        }

        return expr;
    }

    private BinaryExpression ParseExponentiationExpression(Expression expr, in Marker marker)
    {
        _context.IsAssignmentTarget = false;
        _context.IsBindingElement = false;
        var left = expr;
        var right = IsolateCoverGrammar(_parseBinaryExpressionOperand);
        return Finalize(marker, CreateBinaryExpression("**", left, right));
    }

    // https://tc39.github.io/ecma262/#sec-exp-operator
    // https://tc39.github.io/ecma262/#sec-multiplicative-operators
    // https://tc39.github.io/ecma262/#sec-additive-operators
    // https://tc39.github.io/ecma262/#sec-bitwise-shift-operators
    // https://tc39.github.io/ecma262/#sec-relational-operators
    // https://tc39.github.io/ecma262/#sec-equality-operators
    // https://tc39.github.io/ecma262/#sec-binary-bitwise-operators
    // https://tc39.github.io/ecma262/#sec-binary-logical-operators

    private int BinaryPrecedence(in Token token)
    {
        var prec = 0;
        var op = token.Value;

        if (token.Type == TokenType.Punctuator)
        {
            switch ((string?) op)
            {
                case ")":
                case ";":
                case ",":
                case "=":
                case "]":
                    prec = 0;
                    break;

                case "??":
                    prec = 5;
                    break;

                case "||":
                    prec = 6;
                    break;

                case "&&":
                    prec = 7;
                    break;

                case "|":
                    prec = 8;
                    break;

                case "^":
                    prec = 9;
                    break;

                case "&":
                    prec = 10;
                    break;

                case "==":
                case "!=":
                    prec = 11;
                    break;

                case "<":
                case ">":
                case "<=":
                case ">=":
                    prec = 12;
                    break;

                case "<<":
                case ">>":
                    prec = 13;
                    break;

                case "+":
                case "-":
                    prec = 14;
                    break;

                case "*":
                case "/":
                case "%":
                    prec = 15;
                    break;

                default:
                    prec = 0;
                    break;
            }
        }
        else if (token.Type == TokenType.Keyword)
        {
            prec = "instanceof".Equals(op) || _context.AllowIn && "in".Equals(op) ? 12 : 0;
        }

        return prec;
    }

    // pooling for ParseBinaryExpression
    private Stack<Token>? _markersStack;
    private Stack<int>? _precedencesStack;
    private ArrayList<object>? _sharedStack;

    [MethodImpl((MethodImplOptions) 512)]
    private Expression ParseBinaryExpression()
    {
        var startToken = _lookahead;

        var expr = InheritCoverGrammar(_parseBinaryExpressionOperand);

        var allowAndOr = true;
        var allowNullishCoalescing = true;

        static void UpdateNullishCoalescingRestrictions(in Token t, ref bool allowAndOr, ref bool allowNullishCoalescing)
        {
            var value = t.Value;
            if ("&&".Equals(value) || "||".Equals(value))
            {
                allowNullishCoalescing = false;
            }

            if ("??".Equals(value))
            {
                allowAndOr = false;
            }
        }

        var token = _lookahead;
        var prec = BinaryPrecedence(token);
        if (prec > 0)
        {
            UpdateNullishCoalescingRestrictions(token, ref allowAndOr, ref allowNullishCoalescing);
            NextToken();

            _context.IsAssignmentTarget = false;
            _context.IsBindingElement = false;

            var markers = _markersStack ?? new Stack<Token>();
            _markersStack = null;
            markers.Clear();

            markers.Push(startToken);
            markers.Push(_lookahead);

            var left = expr;
            var right = IsolateCoverGrammar(_parseBinaryExpressionOperand);

            var stack = _sharedStack ?? new ArrayList<object>(3);
            _sharedStack = null;
            stack.Clear();

            stack.Add(left);
            stack.Add(token.Value!);
            stack.Add(right);

            var precedences = _precedencesStack ?? new Stack<int>(1);
            _precedencesStack = null;
            precedences.Clear();

            precedences.Push(prec);

            while (true)
            {
                if (IsEndOfFile())
                {
                    TolerateUnexpectedToken(_lookahead);
                    break;
                }

                prec = BinaryPrecedence(_lookahead);
                if (prec <= 0)
                {
                    break;
                }

                if (!allowAndOr && ("&&".Equals(_lookahead.Value) || "||".Equals(_lookahead.Value)) ||
                    !allowNullishCoalescing && "??".Equals(_lookahead.Value))
                {
                    TolerateUnexpectedToken(_lookahead);
                    break;
                }

                UpdateNullishCoalescingRestrictions(_lookahead, ref allowAndOr, ref allowNullishCoalescing);

                // Reduce: make a binary expression from the three topmost entries.
                while (stack.Count > 2 && prec <= precedences.Peek())
                {
                    right = (Expression) stack.Pop();
                    var op = (string) stack.Pop();
                    precedences.Pop();
                    left = (Expression) stack.Pop();
                    markers.Pop();
                    var marker = markers.Peek();
                    var node = StartNode(marker, marker.LineStart);
                    stack.Push(Finalize(node, CreateBinaryExpression(op, left, right)));
                }

                // Shift.
                stack.Push(NextToken().Value!);
                precedences.Push(prec);
                markers.Push(_lookahead);
                stack.Push(IsolateCoverGrammar(_parseBinaryExpressionOperand));
            }

            // Final reduce to clean-up the stack.
            var i = stack.Count - 1;
            expr = (Expression) stack[i];

            var lastMarker = markers.Pop();
            while (i > 1)
            {
                var marker = markers.Pop();
                var lastLineStart = lastMarker.LineStart;
                var node = StartNode(marker, lastLineStart);
                var op = (string) stack[i - 1];
                expr = Finalize(node, CreateBinaryExpression(op, (Expression) stack[i - 2], expr));
                i -= 2;
                lastMarker = marker;
            }

            _markersStack = markers;
            _sharedStack = stack;
            _precedencesStack = precedences;
        }

        return expr;
    }


    // https://tc39.github.io/ecma262/#sec-conditional-operator

    private ConditionalExpression ParseConditionalExpression(Expression expr, in Marker marker)
    {
        var previousAllowIn = _context.AllowIn;
        _context.AllowIn = true;
        var consequent = IsolateCoverGrammar(_parseAssignmentExpression);
        _context.AllowIn = previousAllowIn;

        Expect(":");
        var alternate = IsolateCoverGrammar(_parseAssignmentExpression);

        var conditionalExpression = Finalize(marker, new ConditionalExpression(expr, consequent, alternate));
        _context.IsAssignmentTarget = false;
        _context.IsBindingElement = false;
        return conditionalExpression;
    }

    // https://tc39.github.io/ecma262/#sec-assignment-operators

    private void CheckPatternParam(ref ParsedParameters options, Node param)
    {
        switch (param.Type)
        {
            case Nodes.Identifier:
                ValidateParam(ref options, param, param.As<Identifier>().Name);
                break;
            case Nodes.RestElement:
                CheckPatternParam(ref options, param.As<RestElement>().Argument);
                break;
            case Nodes.AssignmentPattern:
                CheckPatternParam(ref options, param.As<AssignmentPattern>().Left);
                break;
            case Nodes.ArrayPattern:
                ref readonly var list = ref param.As<ArrayPattern>().Elements;
                for (var i = 0; i < list.Count; i++)
                {
                    var element = list[i];
                    if (element != null)
                    {
                        CheckPatternParam(ref options, element);
                    }
                }

                break;
        }

        options.Simple = options.Simple && param is Identifier;
    }

    private ParsedParameters? ReinterpretAsCoverFormalsList(Expression expr)
    {
        ArrayList<Expression> parameters;
        var asyncArrow = false;

        switch (expr.Type)
        {
            case Nodes.Identifier:
                parameters = new ArrayList<Expression>(new Expression[] { expr });
                break;

            default:
                return null;
        }

        var options = new ParsedParameters { Simple = true };

        for (var i = 0; i < parameters.Count; ++i)
        {
            var param = parameters[i];
            if (param.Type == Nodes.AssignmentPattern)
            {
                var assignment = param.As<AssignmentPattern>();
                if (assignment.Right.Type == Nodes.YieldExpression)
                {
                    var yieldExpression = assignment.Right.As<YieldExpression>();
                    if (yieldExpression.Argument != null)
                    {
                        TolerateUnexpectedToken(_lookahead);
                    }

                    assignment._right = new Identifier("yield") { Location = assignment.Right.Location, Range = assignment.Right.Range };
                }
            }
            else if (asyncArrow && param.Type == Nodes.Identifier && param.As<Identifier>().Name == "await")
            {
                TolerateUnexpectedToken(_lookahead);
            }

            CheckPatternParam(ref options, param);
            parameters[i] = param;
        }

        if (_context.Strict || !_context.AllowYield)
        {
            for (var i = 0; i < parameters.Count; ++i)
            {
                var param = parameters[i];
                if (param.Type == Nodes.YieldExpression)
                {
                    TolerateUnexpectedToken(_lookahead);
                }
            }
        }

        if (options.HasDuplicateParameterNames)
        {
            var token = _context.Strict ? options.Stricted : options.FirstRestricted;
            TolerateUnexpectedToken(token ?? default, Messages.DuplicateParameter);
        }

        return new ParsedParameters
        {
            Simple = options.Simple,
            Parameters = parameters,
            Stricted = options.Stricted,
            FirstRestricted = options.FirstRestricted,
            Message = options.Message
        };
    }

    private int _assignmentDepth;

    private protected Expression ParseAssignmentExpression()
    {
        Expression expr;

        if (_assignmentDepth++ > _maxAssignmentDepth)
        {
            TolerateUnexpectedToken(_lookahead, "Maximum statements depth reached");
        }

        if (!_context.AllowYield && MatchKeyword("yield"))
        {
            expr = ParseYieldExpression();
        }
        else
        {
            var token = _lookahead;

            expr = InheritCoverGrammar(_parseBinaryExpression);
            if (ConsumeMatch("?"))
            {
                expr = ParseConditionalExpression(expr, StartNode(token));
            }

            if (MatchAssign())
            {
                if (!_context.IsAssignmentTarget)
                {
                    TolerateError(Messages.InvalidLHSInAssignment);
                }

                Expression left;

                if (!Match("="))
                {
                    _context.IsAssignmentTarget = false;
                    _context.IsBindingElement = false;
                    left = expr;
                }
                else
                {
                    left = ReinterpretExpressionAsPattern(expr);
                }

                var next = NextToken();
                var right = IsolateCoverGrammar(_parseAssignmentExpression);
                expr = Finalize(StartNode(token), new AssignmentExpression((string) next.Value!, left, right));
                _context.FirstCoverInitializedNameError = null;
            }
        }

        _assignmentDepth--;

        return expr;
    }

    // https://tc39.github.io/ecma262/#sec-comma-operator

    private Expression ParseExpression()
    {
        var startToken = _lookahead;
        var expr = IsolateCoverGrammar(_parseAssignmentExpression);

        if (Match(","))
        {
            var expressions = new ArrayList<Expression>();
            expressions.Push(expr);
            while (_lookahead.Type != TokenType.EOF)
            {
                if (!Match(","))
                {
                    break;
                }

                NextToken();
                expressions.Push(IsolateCoverGrammar(_parseAssignmentExpression));
            }

            expr = Finalize(StartNode(startToken), new SequenceExpression(NodeList.From(ref expressions)));
        }

        return expr;
    }

    /// <summary>
    /// Parses the code as a JavaScript expression.
    /// </summary>
    public Expression ParseExpression(string code, bool strict = false)
    {
        Reset(code, source: null);
        try
        {
            _context.Strict = strict;
            _context.IsAsync = true;
            return FinalizeRoot(ParseExpression());
        }
        finally
        {
            ReleaseLargeBuffers();
        }
    }

    // https://tc39.github.io/ecma262/#sec-block

    private Statement ParseStatementListItem()
    {
        Statement statement;

        _context.IsAssignmentTarget = true;
        _context.IsBindingElement = true;

        if (_lookahead.Type == TokenType.Keyword)
        {
            switch ((string?) _lookahead.Value)
            {
                case "import":
                    statement = ParseImportDeclaration();
                    break;
                case "function":
                case "method":
                    statement = ParseSubroutineDeclaration();
                    break;
                case "module":
                    statement = ParseModuleDeclaration();
                    break;
                case "class":
                    statement = ParseClassDeclaration();
                    break;
                default:
                    statement = ParseStatement();
                    break;
            }
        }
        else
        {
            statement = ParseStatement();
        }

        return statement;
    }

    private BlockStatement ParseBlock()
    {
        var node = CreateNode();

        Expect("{");
        var block = new ArrayList<Statement>();
        while (true)
        {
            if (IsEndOfFile())
            {
                TolerateUnexpectedToken(_lookahead);
                break;
            }

            if (Match("}"))
            {
                break;
            }

            block.Add(ParseStatementListItem());
        }

        Expect("}");

        return Finalize(node, new BlockStatement(NodeList.From(ref block)));
    }

    // https://tc39.github.io/ecma262/#sec-let-and-const-declarations

    // pooled for calls which parse a variable/parameter binding
    private ArrayList<Token>? _parseVariableBindingParameters;

    private VariableDeclarator ParseLexicalBinding(VariableDeclarationKind kind, bool inFor)
    {
        var node = CreateNode();
        var parameters = _parseVariableBindingParameters ?? new ArrayList<Token>();
        _parseVariableBindingParameters = null;
        parameters.Clear();

        var id = ParsePattern(ref parameters, kind);

        _parseVariableBindingParameters = parameters;

        Expression? init = null;
        if (!inFor && id.Type != Nodes.Identifier || Match("="))
        {
            Expect("=");
            init = IsolateCoverGrammar(_parseAssignmentExpression);
        }

        return Finalize(node, new VariableDeclarator((Identifier) id!, init));
    }

    private NodeList<VariableDeclarator> ParseBindingList(VariableDeclarationKind kind, bool inFor)
    {
        var list = new ArrayList<VariableDeclarator>(new[] { ParseLexicalBinding(kind, inFor) });

        while (Match(","))
        {
            NextToken();
            list.Add(ParseLexicalBinding(kind, inFor));
        }

        return NodeList.From(ref list);
    }

    private bool IsLexicalDeclaration()
    {
        var state = _scanner.SaveState();
        _scanner.ScanCommentsInternal();
        var next = _scanner.Lex(new LexOptions(_context));
        _scanner.RestoreState(state);

        return next.Type == TokenType.Identifier ||
               next.Type == TokenType.Punctuator && (string?) next.Value == "[" ||
               next.Type == TokenType.Punctuator && (string?) next.Value == "{" ||
               next.Type == TokenType.Keyword && (string?) next.Value == "let" ||
               next.Type == TokenType.Keyword && (string?) next.Value == "yield";
    }


    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private VariableDeclarationKind ParseVariableDeclarationKind(string? kindString)
    {
        VariableDeclarationKind kind = kindString switch
        {
            "var" => VariableDeclarationKind.Var,
            "static" => VariableDeclarationKind.Static,
            "attribute" => VariableDeclarationKind.Attribute,
            "delegate" => VariableDeclarationKind.Delegate,
            _ => VariableDeclarationKind.Invalid
        };

        if (kind == VariableDeclarationKind.Invalid)
            TolerateError("Unknown declaration kind '{0}'", kindString);

        return kind;
    }

    // https://tc39.github.io/ecma262/#sec-destructuring-binding-patterns

    private RestElement ParseBindingRestElement(ref ArrayList<Token> parameters, VariableDeclarationKind? kind)
    {
        var node = CreateNode();

        Expect("...");
        var arg = ParsePattern(ref parameters, kind);

        return Finalize(node, new RestElement(arg));
    }

    private ArrayPattern ParseArrayPattern(ref ArrayList<Token> parameters, VariableDeclarationKind? kind)
    {
        var node = CreateNode();

        Expect("[");
        var elements = new ArrayList<Node?>();
        while (!Match("]"))
        {
            if (IsEndOfFile())
            {
                TolerateUnexpectedToken(_lookahead);
                break;
            }

            if (Match(","))
            {
                NextToken();
                elements.Push(null);
            }
            else
            {
                if (Match("..."))
                {
                    elements.Push(ParseBindingRestElement(ref parameters, kind));
                    break;
                }
                else
                {
                    elements.Push(ParsePatternWithDefault(ref parameters, kind, arrayPattern: true));
                }

                if (!Match("]"))
                {
                    Expect(",");
                }
            }
        }

        Expect("]");

        return Finalize(node, new ArrayPattern(NodeList.From(ref elements)));
    }

    private Expression ParseRestProperty(ref ArrayList<Token> parameters, VariableDeclarationKind? kind)
    {
        var node = CreateNode();
        Expect("...");
        var arg = ParsePattern(ref parameters);
        if (Match("="))
        {
            TolerateError(Messages.DefaultRestProperty);
            return Finalize(node, new ErrorExpression());
        }

        if (!Match("}"))
        {
            TolerateError(Messages.PropertyAfterRestProperty);
            return Finalize(node, new ErrorExpression());
        }

        return Finalize(node, new RestElement(arg));
    }

    private Expression ParsePattern(ref ArrayList<Token> parameters, VariableDeclarationKind? kind = null, bool arrayPattern = false)
    {
        Expression pattern;

        if (Match("["))
        {
            pattern = ParseArrayPattern(ref parameters, kind);
        }
        else if (Match("{"))
        {
            // pattern = ParseObjectPattern(ref parameters, kind);
            pattern = ParseListAssignmentNestedElementList(); // ADHOC: Allow function test(a, {b, c})
        }
        else
        {
            // ADHOC: Adhoc allows deconstruction directly into attributes and static paths
            if (arrayPattern)
            {
                parameters.Push(_lookahead);
                pattern = ParseLeftHandSideExpression();

            }
            else
            {
                pattern = ParseVariableIdentifier(kind);
            }
        }

        return pattern;
    }

    private Expression ParsePatternWithDefault(ref ArrayList<Token> parameters, VariableDeclarationKind? kind = null, bool arrayPattern = false)
    {
        var startToken = _lookahead;

        var pattern = ParsePattern(ref parameters, kind, arrayPattern);
        if (Match("="))
        {
            NextToken();
            var previousAllowYield = _context.AllowYield;
            _context.AllowYield = true;
            var right = IsolateCoverGrammar(_parseAssignmentExpression);
            _context.AllowYield = previousAllowYield;
            pattern = Finalize(StartNode(startToken), new AssignmentPattern(pattern, right));
        }

        return pattern;
    }

    // https://tc39.github.io/ecma262/#sec-variable-statement

    private Identifier ParseVariableIdentifier(VariableDeclarationKind? kind = null, bool allowAwaitKeyword = false)
    {
        var node = CreateNode();

        var token = NextToken();
        if (token.Type == TokenType.Keyword && (string?) token.Value == "yield")
        {
            if (_context.Strict)
            {
                TolerateUnexpectedToken(token, Messages.StrictReservedWord);
            }

            if (!_context.AllowYield)
            {
                TolerateUnexpectedToken(token);
            }
        }
        else if (token.Type == TokenType.Keyword && (string?) token.Value == "import") // ADHOC HACK: (gt6/SaveDataUtil.ad, function import())
        {
            return Finalize(node, new Identifier((string) token.Value!));
        }
        else if (token.Type != TokenType.Identifier)
        {
            var stringValue = token.Value as string;
            if (_context.Strict || stringValue == null || stringValue != "let" || kind != VariableDeclarationKind.Var)
            {
                TolerateUnexpectedToken(token);
            }
        }
        else if (_context.IsAsync && !allowAwaitKeyword && token.Type == TokenType.Identifier && (string?) token.Value == "await")
        {
            TolerateUnexpectedToken(token);
        }

        return Finalize(node, new Identifier((string) token.Value!));
    }

    private VariableDeclarator ParseVariableDeclaration(bool inFor)
    {
        var node = CreateNode();

        var parameters = _parseVariableBindingParameters ?? new ArrayList<Token>();
        _parseVariableBindingParameters = null;
        parameters.Clear();

        var id = ParsePattern(ref parameters, VariableDeclarationKind.Var);
        _parseVariableBindingParameters = parameters;

        Expression? init = null;
        if (Match("="))
        {
            NextToken();
            init = IsolateCoverGrammar(_parseAssignmentExpression);
        }
        else if (id.Type != Nodes.Identifier && !inFor)
        {
            Expect("=");
        }

        return Finalize(node, new VariableDeclarator((Identifier) id, init));
    }

    private NodeList<VariableDeclarator> ParseVariableDeclarationList(bool inFor)
    {
        var list = new ArrayList<VariableDeclarator>(new[] { ParseVariableDeclaration(inFor) });

        while (Match(","))
        {
            if (IsEndOfFile())
            {
                TolerateUnexpectedToken(_lookahead);
                break;
            }

            NextToken();
            list.Push(ParseVariableDeclaration(inFor));
        }

        return NodeList.From(ref list);
    }

    private VariableDeclaration ParseVariableStatement()
    {
        var node = CreateNode();
        ExpectKeyword("var");
        var declarations = ParseVariableDeclarationList(inFor: false);
        ConsumeSemicolon();

        return Finalize(node, new VariableDeclaration(declarations, VariableDeclarationKind.Var));
    }

    // https://tc39.github.io/ecma262/#sec-empty-statement

    private EmptyStatement ParseEmptyStatement()
    {
        var node = CreateNode();
        Expect(";");
        return Finalize(node, new EmptyStatement());
    }

    // https://tc39.github.io/ecma262/#sec-expression-statement

    private ExpressionStatement ParseExpressionStatement()
    {
        var node = CreateNode();
        var expr = ParseExpression();
        ConsumeSemicolon();
        return Finalize(node, new ExpressionStatement(expr));
    }

    // https://tc39.github.io/ecma262/#sec-if-statement

    private Statement ParseIfClause()
    {
        if (_context.Strict && MatchKeyword("function"))
        {
            TolerateError(Messages.StrictFunction);
        }

        return ParseStatement();
    }

    private IfStatement ParseIfStatement()
    {
        var node = CreateNode();
        Statement consequent;
        Statement? alternate = null;

        ExpectKeyword("if");
        Expect("(");
        var test = ParseExpression();

        if (!Match(")") && _tolerant)
        {
            var consequentNode = CreateNode();
            TolerateUnexpectedToken(NextToken());
            consequent = Finalize(consequentNode, new EmptyStatement());
        }
        else
        {
            Expect(")");
            consequent = ParseIfClause();
            if (MatchKeyword("else"))
            {
                NextToken();
                alternate = ParseIfClause();
            }
        }

        return Finalize(node, new IfStatement(test, consequent, alternate));
    }

    // https://tc39.github.io/ecma262/#sec-do-while-statement

    private DoWhileStatement ParseDoWhileStatement()
    {
        var node = CreateNode();
        ExpectKeyword("do");

        if (MatchKeyword("class") || MatchKeyword("function"))
        {
            TolerateUnexpectedToken(_lookahead);
        }

        var previousInIteration = _context.InIteration;
        _context.InIteration = true;
        var body = ParseStatement();
        _context.InIteration = previousInIteration;

        ExpectKeyword("while");
        Expect("(");
        var test = ParseExpression();

        if (!Match(")") && _tolerant)
        {
            TolerateUnexpectedToken(NextToken());
        }
        else
        {
            Expect(")");
            if (Match(";"))
            {
                NextToken();
            }
        }

        return Finalize(node, new DoWhileStatement(body, test));
    }

    // https://tc39.github.io/ecma262/#sec-while-statement

    private WhileStatement ParseWhileStatement()
    {
        var node = CreateNode();
        Statement body;

        ExpectKeyword("while");
        Expect("(");
        var test = ParseExpression();

        if (!Match(")") && _tolerant)
        {
            TolerateUnexpectedToken(NextToken());
            body = Finalize(CreateNode(), new EmptyStatement());
        }
        else
        {
            Expect(")");

            var previousInIteration = _context.InIteration;
            _context.InIteration = true;
            body = ParseStatement();
            _context.InIteration = previousInIteration;
        }

        return Finalize(node, new WhileStatement(test, body));
    }

    // https://tc39.github.io/ecma262/#sec-for-statement
    // https://tc39.github.io/ecma262/#sec-for-in-and-for-of-statements

    private Statement ParseForStatement()
    {
        StatementListItem? init = null;
        Expression? test = null;
        Expression? update = null;
        Node? left = null;
        Expression? right = null;
        var @await = false;

        var node = CreateNode();

        string? forKeyword = _lookahead.Value as string;
        ExpectKeyword("for", "foreach");

        if (MatchContextualKeyword("await"))
        {
            if (!_context.IsAsync)
            {
                TolerateUnexpectedToken(_lookahead);
            }

            @await = true;
            NextToken();
        }

        Expect("(");

        if (Match(";"))
        {
            if (@await)
            {
                TolerateUnexpectedToken(_lookahead);
            }

            NextToken();
        }
        else
        {
            if (MatchKeyword("var"))
            {
                var initNode = CreateNode();
                NextToken();

                var previousAllowIn = _context.AllowIn;
                _context.AllowIn = false;

                var declarations = ParseVariableDeclarationList(true);
                _context.AllowIn = previousAllowIn;

                if (forKeyword!.Equals("foreach", StringComparison.OrdinalIgnoreCase) && declarations.Count == 1 && declarations[0]!.Init == null && MatchContextualKeyword("in"))
                {
                    left = Finalize(initNode, new VariableDeclaration(declarations, VariableDeclarationKind.Var));
                    NextToken();
                    right = ParseAssignmentExpression();
                    init = null;
                }
                else
                {
                    if (@await)
                    {
                        TolerateUnexpectedToken(_lookahead);
                    }

                    init = Finalize(initNode, new VariableDeclaration(declarations, VariableDeclarationKind.Var));
                    Expect(";");
                }
            }
            else if (Match("|")) // ADHOC: LIST_ASSIGN
            {
                var initNode = CreateNode();

                var previousAllowIn = _context.AllowIn;
                _context.AllowIn = false;
                var variableList = ParseListAssignmentElementList();
                _context.AllowIn = previousAllowIn;

                if (forKeyword?.Equals("foreach", StringComparison.OrdinalIgnoreCase) == true && MatchContextualKeyword("in"))
                {
                    left = Finalize(initNode, variableList);
                    NextToken();

                    right = ParseAssignmentExpression();
                    init = null;
                }
            }
            else
            {
                var initStartToken = _lookahead;
                var previousIsBindingElement = _context.IsBindingElement;
                var previousIsAssignmentTarget = _context.IsAssignmentTarget;
                var previousFirstCoverInitializedNameError = _context.FirstCoverInitializedNameError;

                var previousAllowIn = _context.AllowIn;
                _context.AllowIn = false;
                init = InheritCoverGrammar(_parseAssignmentExpression);
                _context.AllowIn = previousAllowIn;

                if (MatchContextualKeyword("in"))
                {
                    if (@await)
                    {
                        TolerateUnexpectedToken(_lookahead);
                    }

                    if (!_context.IsAssignmentTarget || init.Type == Nodes.AssignmentExpression)
                    {
                        TolerateError(Messages.InvalidLHSInForIn);
                    }

                    NextToken();
                    left = ReinterpretExpressionAsPattern((Expression) init);
                    right = ParseExpression();
                    init = null;
                }
                else
                {
                    if (@await)
                    {
                        TolerateUnexpectedToken(_lookahead);
                    }

                    // The `init` node was not parsed isolated, but we would have wanted it to.
                    _context.IsBindingElement = previousIsBindingElement;
                    _context.IsAssignmentTarget = previousIsAssignmentTarget;
                    _context.FirstCoverInitializedNameError = previousFirstCoverInitializedNameError;

                    if (Match(","))
                    {
                        var initSeq = new ArrayList<Expression>(new[] { (Expression) init });
                        while (Match(","))
                        {
                            if (IsEndOfFile())
                            {
                                TolerateUnexpectedToken(_lookahead);
                                break;
                            }

                            NextToken();
                            initSeq.Push(IsolateCoverGrammar(_parseAssignmentExpression));
                        }

                        init = Finalize(StartNode(initStartToken), new SequenceExpression(NodeList.From(ref initSeq)));
                    }

                    Expect(";");
                }
            }
        }

        if (left == null)
        {
            if (!Match(";"))
            {
                test = IsolateCoverGrammar(_parseExpression);
            }

            Expect(";");
            if (!Match(")"))
            {
                update = IsolateCoverGrammar(_parseExpression);
            }
        }

        Statement body;
        if (!Match(")") && _tolerant)
        {
            TolerateUnexpectedToken(NextToken());
            body = Finalize(CreateNode(), new EmptyStatement());
        }
        else
        {
            Expect(")");

            TolerateInvalidLoopStatement();

            var previousInIteration = _context.InIteration;
            _context.InIteration = true;
            body = IsolateCoverGrammar(_parseStatement);
            _context.InIteration = previousInIteration;
        }

        return left == null
                ? Finalize(node, new ForStatement(init, test, update, body))
                : Finalize(node, new ForeachStatement(left, right!, body));
    }

    // https://tc39.github.io/ecma262/#sec-continue-statement

    private Statement ParseContinueStatement()
    {
        var node = CreateNode();
        ExpectKeyword("continue");

        Identifier? label = null;
        if (_lookahead.Type == TokenType.Identifier && !_hasLineTerminator)
        {
            label = ParseVariableIdentifier();

            var key = label.Name;
            if (!_context.LabelSet.Contains(key))
            {
                TolerateError(Messages.UnknownLabel, label.Name);
                return Finalize(node, new ErrorStatement());
            }
        }

        ConsumeSemicolon();
        if (label == null && !_context.InIteration)
        {
            TolerateError(Messages.IllegalContinue);
            return Finalize(node, new ErrorStatement());
        }

        return Finalize(node, new ContinueStatement(label));
    }

    // https://tc39.github.io/ecma262/#sec-break-statement

    private Statement ParseBreakStatement()
    {
        var node = CreateNode();
        ExpectKeyword("break");

        Identifier? label = null;
        if (_lookahead.Type == TokenType.Identifier && !_hasLineTerminator)
        {
            label = ParseVariableIdentifier();

            var key = label.Name;
            if (!_context.LabelSet.Contains(key))
            {
                TolerateError(Messages.UnknownLabel, label.Name);
                return Finalize(node, new ErrorStatement());
            }
        }

        ConsumeSemicolon();
        if (label == null && !_context.InIteration && !_context.InSwitch)
        {
            TolerateError(Messages.IllegalBreak);
            return Finalize(node, new ErrorStatement());
        }

        return Finalize(node, new BreakStatement(label));
    }

    // https://tc39.github.io/ecma262/#sec-return-statement

    private ReturnStatement ParseReturnStatement()
    {
        var node = CreateNode();
        ExpectKeyword("return");

        var hasArgument = !Match(";") && !Match("}") &&
                          !_hasLineTerminator && _lookahead.Type != TokenType.EOF ||
                          _lookahead.Type == TokenType.StringLiteral ||
                          _lookahead.Type == TokenType.Template;

        var argument = hasArgument ? ParseExpression() : null;
        ConsumeSemicolon();

        return Finalize(node, new ReturnStatement(argument));
    }

    // https://tc39.github.io/ecma262/#sec-switch-statement

    private SwitchCase ParseSwitchCase()
    {
        var node = CreateNode();

        Expression? test;
        if (MatchKeyword("default"))
        {
            NextToken();
            test = null;
        }
        else
        {
            ExpectKeyword("case");
            test = ParseExpression();
        }

        Expect(":");

        var consequent = new ArrayList<Statement>();
        while (true)
        {
            if (IsEndOfFile())
            {
                TolerateUnexpectedToken(_lookahead);
                break;
            }

            if (Match("}") || MatchKeyword("default") || MatchKeyword("case"))
            {
                break;
            }

            consequent.Push(ParseStatementListItem());
        }

        return Finalize(node, new SwitchCase(test, NodeList.From(ref consequent)));
    }

    private SwitchStatement ParseSwitchStatement()
    {
        var node = CreateNode();
        ExpectKeyword("switch");

        Expect("(");
        var discriminant = ParseExpression();
        Expect(")");

        var previousInSwitch = _context.InSwitch;
        _context.InSwitch = true;

        var cases = new ArrayList<SwitchCase>();
        var defaultFound = false;
        Expect("{");
        while (true)
        {
            if (Match("}") || IsEndOfFile())
            {
                break;
            }

            var clause = ParseSwitchCase();
            if (clause.Test == null)
            {
                if (defaultFound)
                {
                    TolerateError(Messages.MultipleDefaultsInSwitch);
                }

                defaultFound = true;
            }

            cases.Push(clause);
        }

        Expect("}");

        _context.InSwitch = previousInSwitch;

        return Finalize(node, new SwitchStatement(discriminant, NodeList.From(ref cases)));
    }

    // ECMA-262 13.13 Labelled Statements

    private Statement ParseLabelledStatement()
    {
        var node = CreateNode();
        var expr = ParseExpression();

        Statement statement;
        if (expr.Type == Nodes.Identifier && Match(":"))
        {
            NextToken();

            var id = expr.As<Identifier>();
            var key = id.Name;
            if (!_context.LabelSet.Add(key))
            {
                TolerateError(Messages.Redeclaration, "Label", id.Name);
            }

            Statement body;
            if (MatchKeyword("class"))
            {
                TolerateUnexpectedToken(_lookahead);
                body = ParseClassDeclaration();
            }
            else if (MatchKeyword("module"))
            {
                TolerateUnexpectedToken(_lookahead);
                body = ParseModuleDeclaration();
            }
            if (MatchKeyword("function"))
            {
                var token = _lookahead;
                var declaration = ParseSubroutineDeclaration();
                if (_context.Strict)
                {
                    TolerateUnexpectedToken(token, Messages.StrictFunction);
                }

                body = declaration;
            }
            else
            {
                body = ParseStatement();
            }

            _context.LabelSet.Remove(key);

            statement = new LabeledStatement(id, body);
        }
        else
        {
            ConsumeSemicolon();
            statement = new ExpressionStatement(expr);
        }

        return Finalize(node, statement);
    }

    // https://tc39.github.io/ecma262/#sec-throw-statement

    private ThrowStatement ParseThrowStatement()
    {
        var node = CreateNode();
        ExpectKeyword("throw");

        if (_hasLineTerminator)
        {
            TolerateError(Messages.NewlineAfterThrow);
        }

        var argument = ParseExpression();
        ConsumeSemicolon();

        return Finalize(node, new ThrowStatement(argument));
    }

    // https://tc39.github.io/ecma262/#sec-try-statement

    private CatchClause ParseCatchClause()
    {
        var node = CreateNode();

        ExpectKeyword("catch");

        Node? param = null;
        if (Match("("))
        {
            Expect("(");
            if (Match(")"))
            {
                TolerateUnexpectedToken(_lookahead);
            }

            var parameters = _parseVariableBindingParameters ?? new ArrayList<Token>();
            _parseVariableBindingParameters = null;
            parameters.Clear();

            param = ParsePattern(ref parameters);
            var paramMap = new Dictionary<string?, bool>();
            for (var i = 0; i < parameters.Count; i++)
            {
                var key = (string?) parameters[i].Value;
                if (paramMap.ContainsKey(key))
                {
                    TolerateError(Messages.DuplicateBinding, parameters[i].Value);
                }

                paramMap[key] = true;
            }

            _parseVariableBindingParameters = parameters;

            Expect(")");
        }

        var body = ParseBlock();

        return Finalize(node, new CatchClause(param, body));
    }

    private BlockStatement ParseFinallyClause()
    {
        ExpectKeyword("finally");
        return ParseBlock();
    }

    private TryStatement ParseTryStatement()
    {
        var node = CreateNode();
        ExpectKeyword("try");

        var block = ParseBlock();
        var handler = MatchKeyword("catch") ? ParseCatchClause() : null;

        // Adhoc: finalizer not part of it.

        return Finalize(node, new TryStatement(block, handler));
    }

    // https://tc39.github.io/ecma262/#sec-ecmascript-language-statements-and-declarations

    private Statement ParseStatement()
    {
        Statement? statement;
        switch (_lookahead.Type)
        {
            case TokenType.BooleanLiteral:
            case TokenType.NilLiteral:
            case TokenType.NumericLiteral:
            case TokenType.StringLiteral:
            case TokenType.Template:
                statement = ParseExpressionStatement();
                break;

            case TokenType.Punctuator:
                switch ((string?) _lookahead.Value)
                {
                    case "{":
                        statement = ParseBlock();
                        break;
                    case "|": // ADHOC: LIST_ASSIGN
                        statement = ParseListAssignment();
                        break;
                    case ";":
                        statement = ParseEmptyStatement();
                        break;
                    case "@":
                        statement = ParsePragmaStatement();
                        break;
                    case "#":
                        statement = ParsePreprocessorDirectiveStatement();
                        break;
                    case "(":
                    default:
                        statement = ParseExpressionStatement();
                        break;
                }

                break;

            case TokenType.Identifier:
                statement = MatchAsyncFunction() ? ParseSubroutineDeclaration() : ParseLabelledStatement();
                break;

            case TokenType.Keyword:
                switch ((string?) _lookahead.Value)
                {
                    case "break":
                        statement = ParseBreakStatement();
                        break;
                    case "continue":
                        statement = ParseContinueStatement();
                        break;
                    case "do":
                        statement = ParseDoWhileStatement();
                        break;
                    case "for":
                    case "foreach":
                        statement = ParseForStatement();
                        break;
                    case "function":
                        statement = ParseSubroutineDeclaration();
                        break;
                    case "finally": // Adhoc: Scope/module finalizer
                        statement = ParseFinalizer();
                        break;
                    case "if":
                        statement = ParseIfStatement();
                        break;
                    case "return":
                        statement = ParseReturnStatement();
                        break;
                    case "switch":
                        statement = ParseSwitchStatement();
                        break;
                    case "undef":
                        statement = ParseUndefStatement();
                        break;
                    case "throw":
                        statement = ParseThrowStatement();
                        break;
                    case "try":
                        statement = ParseTryStatement();
                        break;
                    case "var":
                        statement = ParseVariableStatement();
                        break;
                    case "while":
                        statement = ParseWhileStatement();
                        break;

                    case "require":
                        statement = ParseRequireStatement();
                        break;

                    case "static": // ADHOC
                        statement = ParseStaticStatement();
                        break;
                    case "attribute": // ADHOC
                        statement = ParseAttributeStatement();
                        break;
                    case "delegate": // ADHOC
                        statement = ParseDelegateDeclaration();
                        break;
                    case "print": // ADHOC
                        statement = ParsePrintStatement();
                        break;

                    default:
                        {
                            var node = CreateNode();
                            statement = Finalize(node, ParseExpressionStatement());
                        }
                        break;
                }

                break;

            default:
                TolerateUnexpectedToken(_lookahead);
                NextToken();

                return new ErrorStatement();
        }

        return statement;
    }

    private ExpressionStatement ParseDirective()
    {
        var node = CreateNode();
        var expr = ParseExpression();

        ConsumeSemicolon();

        return Finalize(node, new ExpressionStatement(expr));
    }

    private ArrayList<Statement> ParseDirectivePrologues()
    {
        Token? firstRestricted = null;

        var body = new ArrayList<Statement>();
        while (!IsEndOfFile())
        {
            var token = _lookahead;

            if (firstRestricted == null && token.OctalKind != LegacyOctalKind.None)
            {
                firstRestricted = token;
            }

            if (token.Type != TokenType.StringLiteral)
            {
                break;
            }

            var statement = ParseDirective();
            body.Push(statement);
        }

        if (_context.Strict && firstRestricted != null)
        {
            TolerateUnexpectedToken(firstRestricted.Value, firstRestricted.Value.OctalKind == LegacyOctalKind.Escaped8or9
                ? Messages.StrictEscape89
                : Messages.StrictOctalLiteral);
        }

        return body;
    }

    // https://tc39.github.io/ecma262/#sec-function-definitions

    private BlockStatement ParseFunctionSourceElements()
    {
        var node = CreateNode();

        Expect("{");
        var body = ParseDirectivePrologues();

        var previousLabelSetEmpty = _context.LabelSet.Count == 0;
        var previousLabelSet = _context.LabelSet;
        var previousInIteration = _context.InIteration;
        var previousInSwitch = _context.InSwitch;
        var previousInFunctionBody = _context.InFunctionBody;

        _context.LabelSet = previousLabelSetEmpty ? previousLabelSet : new HashSet<string?>();
        _context.InIteration = false;
        _context.InSwitch = false;
        _context.InFunctionBody = true;

        while (_lookahead.Type != TokenType.EOF)
        {
            if (Match("}"))
            {
                break;
            }

            body.Push(ParseStatementListItem());
        }

        Expect("}");

        _context.LabelSet = previousLabelSet;
        if (previousLabelSetEmpty)
        {
            _context.LabelSet.Clear();
        }

        _context.InIteration = previousInIteration;
        _context.InSwitch = previousInSwitch;
        _context.InFunctionBody = previousInFunctionBody;

        return Finalize(node, new BlockStatement(NodeList.From(ref body)));
    }

    private void ValidateParam(ref ParsedParameters options, Node param, string? name)
    {
        var key = name;
        if (_context.Strict)
        {
            if (options.ParamSetContains(key))
            {
                options.Stricted = new Token(); // Marker token
                options.HasDuplicateParameterNames = true;
            }
        }
        else if (options.FirstRestricted == null)
        {
            if (options.ParamSetContains(key))
            {
                options.Stricted = new Token(); // Marker token
                options.HasDuplicateParameterNames = true;
            }
        }

        options.ParamSetAdd(key);
    }

    private void ValidateParam2(ref ParsedParameters options, in Token param, string? name)
    {
        var key = name;
        if (_context.Strict)
        {
            if (options.ParamSetContains(key))
            {
                options.Stricted = param;
                options.HasDuplicateParameterNames = true;
            }
        }
        else if (options.FirstRestricted == null)
        {
            if (options.ParamSetContains(key))
            {
                options.Stricted = param;
                options.HasDuplicateParameterNames = true;
            }
        }

        options.ParamSetAdd(key);
    }

    private RestElement ParseRestElement(ref ArrayList<Token> parameters)
    {
        var node = CreateNode();


        Expect("...");
        var arg = ParsePattern(ref parameters);
        if (Match("="))
        {
            TolerateError(Messages.DefaultRestParameter);
        }

        if (!Match(")"))
        {
            TolerateError(Messages.ParameterAfterRestParameter);
        }

        return Finalize(node, new RestElement(arg));
    }

    private void ParseFormalParameter(ref ParsedParameters options)
    {
        var parameters = _parseVariableBindingParameters ?? new ArrayList<Token>();
        _parseVariableBindingParameters = null;
        parameters.Clear();

        var param = ParsePatternWithDefault(ref parameters);
        if (Match("...")) // ADHOC
        {
            Expect("...");

            var node = CreateNode();
            param = Finalize(node, new RestElement(param));
        }

        for (var i = 0; i < parameters.Count; i++)
        {
            ValidateParam2(ref options, parameters[i], (string?) parameters[i].Value);
        }

        options.Simple = options.Simple && param is Identifier;
        options.Parameters.Push(param);

        _parseVariableBindingParameters = parameters;
    }

    private ParsedParameters ParseFormalParameters(Token? firstRestricted = null)
    {
        var options = new ParsedParameters { Simple = true, FirstRestricted = firstRestricted };

        Expect("(");
        if (!Match(")"))
        {
            options.Parameters = new ArrayList<Expression>();
            while (!IsEndOfFile())
            {
                ParseFormalParameter(ref options);
                if (Match(")"))
                {
                    break;
                }

                Expect(",");
                if (Match(")"))
                {
                    break;
                }
            }
        }

        Expect(")");

        if (options.HasDuplicateParameterNames && (_context.Strict || !options.Simple))
        {
            TolerateError(Messages.DuplicateParameter);
        }

        return new ParsedParameters
        {
            Simple = options.Simple,
            Parameters = options.Parameters,
            Stricted = options.Stricted,
            FirstRestricted = options.FirstRestricted,
            Message = options.Message
        };
    }

    private bool MatchAsyncFunction()
    {
        [MethodImpl(MethodImplOptions.NoInlining)]
        static bool ValidateMatch(Scanner scanner, Context context)
        {
            var state = scanner.SaveState();
            scanner.ScanCommentsInternal();
            var next = scanner.Lex(new LexOptions(context));
            scanner.RestoreState(state);

            return state.LineNumber == next.LineNumber && next.Type == TokenType.Keyword && ((string?) next.Value == "function" || (string?) next.Value == "method");
        }

        return MatchContextualKeyword("async") && ValidateMatch(_scanner, _context);
    }

    private Declaration ParseSubroutineDeclaration(bool identifierIsOptional = false)
    {
        var node = CreateNode();
        var isAsync = MatchContextualKeyword("async");
        if (isAsync)
        {
            if (_context.InIteration)
            {
                TolerateError(Messages.AsyncFunctionInSingleStatementContext);
            }

            NextToken();
        }

        bool isMethod = MatchKeyword("method");
        if (!MatchKeyword("function") && !isMethod)
            TolerateUnexpectedToken(_lookahead);
        NextToken();

        var isGenerator = Match("*");
        if (isGenerator)
        {
            NextToken();
        }

        string? message = null;
        Identifier? id = null;
        Token? firstRestricted = null;

        if (!identifierIsOptional || !Match("("))
        {
            var token = _lookahead;
            id = ParseVariableIdentifier();
        }

        var previousInClassStaticBlock = _context.InClassStaticBlock;
        var previousIsAsync = _context.IsAsync;
        var previousAllowYield = _context.AllowYield;
        _context.InClassStaticBlock = false;
        _context.IsAsync = isAsync;
        _context.AllowYield = !isGenerator;

        var formalParameters = ParseFormalParameters(firstRestricted);
        var parameters = NodeList.From(ref formalParameters.Parameters);
        var stricted = formalParameters.Stricted;
        firstRestricted = formalParameters.FirstRestricted;
        if (formalParameters.Message != null)
        {
            message = formalParameters.Message;
        }

        var previousStrict = _context.Strict;
        var previousAllowStrictDirective = _context.AllowStrictDirective;
        _context.AllowStrictDirective = formalParameters.Simple;
        var body = ParseFunctionSourceElements();
        if (_context.Strict && firstRestricted != null)
        {
            TolerateUnexpectedToken(firstRestricted.Value, message);
        }

        if (_context.Strict && stricted != null)
        {
            TolerateUnexpectedToken(stricted.Value, message);
        }

        var hasStrictDirective = _context.Strict;
        _context.AllowStrictDirective = previousAllowStrictDirective;
        _context.Strict = previousStrict;
        _context.IsAsync = previousIsAsync;
        _context.AllowYield = previousAllowYield;
        _context.InClassStaticBlock = previousInClassStaticBlock;

        if (!isMethod)
            return Finalize(node, new FunctionDeclaration(id, parameters, body, isGenerator, hasStrictDirective, isAsync));
        else
            return Finalize(node, new MethodDeclaration(id, parameters, body, isGenerator, hasStrictDirective, isAsync));
    }

    // https://tc39.github.io/ecma262/#sec-method-definitions

    private static bool QualifiedPropertyName(in Token token)
    {
        return token.Type switch
        {
            TokenType.Identifier or
            TokenType.StringLiteral or
            TokenType.BooleanLiteral or
            TokenType.NilLiteral or
            TokenType.NumericLiteral or
            TokenType.Keyword => true,
            TokenType.Punctuator => "[".Equals(token.Value) || "#".Equals(token.Value),
            _ => false
        };
    }

    // https://tc39.github.io/ecma262/#sec-generator-function-definitions

    [StringMatcher("[", "(", "{", "+", "-", "!", "~", "++", "--", "/", "/=")]
    private static partial bool IsPunctuatorExpressionStart(string input);

    [StringMatcher("class", "delete", "function", "import", "new", "super", "this", "typeof", "void", "yield")]
    private static partial bool IsKeywordExpressionStart(string input);

    private protected virtual bool IsStartOfExpression()
    {
        var start = true;

        if (_lookahead.Value is not string value)
        {
            return start;
        }

        if (_lookahead.Type == TokenType.Punctuator)
        {
            start = IsPunctuatorExpressionStart(value);
        }
        else if (_lookahead.Type == TokenType.Keyword)
        {
            start = IsKeywordExpressionStart(value) || !_context.IsModule && value == "let";
        }

        return start;
    }

    private YieldExpression ParseYieldExpression()
    {
        var node = CreateNode();
        ExpectKeyword("yield");

        Expression? argument = null;
        var delegat = false;
        if (!_hasLineTerminator)
        {
            var previousAllowYield = _context.AllowYield;
            _context.AllowYield = false;
            delegat = Match("*");
            if (delegat)
            {
                NextToken();
                argument = ParseAssignmentExpression();
            }
            else if (IsStartOfExpression())
            {
                argument = ParseAssignmentExpression();
            }

            _context.AllowYield = previousAllowYield;
        }

        return Finalize(node, new YieldExpression(argument, delegat));
    }

    // ADHOC ZONE

    #region Adhoc

    private Expression ParseVariableIdentifierAllowStatic(VariableDeclarationKind? kind = null)
    {
        var node = CreateNode();
        bool isTopLevelScopeResolution = Match("::");
        if (isTopLevelScopeResolution)
            NextToken();

        var token = NextToken();

        if (token.Type != TokenType.Identifier)
        {
            var stringValue = token.Value as string;
            if (_context.Strict || stringValue == null || kind != VariableDeclarationKind.Var)
            {
                TolerateUnexpectedToken(token);
            }
        }
        else if ((_context.IsModule || _context.IsAsync) && token.Type == TokenType.Identifier && (string?) token.Value == "await")
        {
            TolerateUnexpectedToken(token);
        }

        string? str = (string?) token.Value;
        while ((string?) _lookahead.Value == "::")
        {
            if (IsEndOfFile())
            {
                TolerateUnexpectedToken(_lookahead);
                break;
            }

            NextToken();
            token = NextToken();
            str += "::";
            str += token.Value;
        }

        // TODO Fix
        if (!string.IsNullOrEmpty(str) && str!.EndsWith("::", StringComparison.OrdinalIgnoreCase))
            str = str.Substring(0, str.Length - 2);

        var id = new Identifier(str!);
        if (isTopLevelScopeResolution)
            return Finalize(node, new StaticIdentifier(id));
        else
            return Finalize(node, id);
    }

    private ClassDeclaration ParseClassDeclaration(bool identifierIsOptional = false)
    {
        var node = CreateNode();

        var previousStrict = _context.Strict;
        var previousAllowSuper = _context.AllowSuperAccess;
        _context.Strict = true;

        ExpectKeyword("class");

        // Regular class or module declaration
        var id = identifierIsOptional && _lookahead.Type != TokenType.Identifier
            ? null
            : ParseVariableIdentifierAllowStatic();

        Expression? superClass = null;

        // ADHOC: ':' instead of extends
        if (Match(":"))
        {
            NextToken();
            superClass = IsolateCoverGrammar(ParseStaticIdentifierName);
            _context.AllowSuperAccess = true;
        }

        var classBody = ParseBlock();
        _context.Strict = previousStrict;
        _context.AllowSuperAccess = previousAllowSuper;

        return Finalize(node, new ClassDeclaration(id, superClass, classBody));
    }

    private Identifier ParseStaticIdentifierName()
    {
        var node = CreateNode();
        var token = NextToken();
        if (!IsIdentifierName(token))
        {
            TolerateUnexpectedToken(token);
            return Finalize(node, new Identifier(string.Empty));
        }

        string id = (string) token.Value!;

        while (Match("::"))
        {
            if (IsEndOfFile())
            {
                TolerateUnexpectedToken(_lookahead);
                break;
            }

            id += NextToken().Value as string;
            token = NextToken();

            if (!IsIdentifierName(token))
            {
                TolerateUnexpectedToken(_lookahead);
                break;
            }

            id += token.Value as string;
        }

        return Finalize(node, new Identifier(id));
    }

    private ImportDefaultSpecifier ParseImportSpecifier()
    {
        var node = CreateNode();
        var local = ParseIdentifierName();
        return Finalize(node, new ImportDefaultSpecifier(local));
    }

    // import *;
    private Identifier ParseImportAll()
    {
        var node = CreateNode();

        Expect("*");
        NextToken();

        return Finalize(node, new Identifier("*"));
    }

    private ImportDeclaration ParseImportDeclaration() // ADHOC
    {
        var node = CreateNode();
        ExpectKeyword("import");

        var namespacePath = new ArrayList<ImportDeclarationSpecifier>();

        Identifier? target = null;
        Identifier? alias = null;
        if (IsIdentifierName(_lookahead))
        {
            ImportDefaultSpecifier mainNamespace = ParseImportSpecifier();
            namespacePath.Push(mainNamespace);

            // import foo
            while (Match("::") || Match("*"))
            {
                if (IsEndOfFile())
                {
                    TolerateUnexpectedToken(_lookahead);
                    break;
                }

                bool isStaticPathAccess = Match("::");

                if (isStaticPathAccess)
                {
                    NextToken();
                    if (Match("*"))
                    {
                        target = ParseImportAll();
                        break;
                    }
                    else if (_lookahead.Type == TokenType.Identifier)
                    {
                        namespacePath.Push(ParseImportSpecifier());
                        continue;
                    }
                    else
                        TolerateUnexpectedToken(_lookahead);
                }
                else if (Match("*"))
                {
                    target = ParseImportAll();
                    break;
                }

                NextToken();
            }
        }
        else
        {
            TolerateUnexpectedToken(NextToken());
        }

        if (MatchContextualKeyword("as"))
        {
            NextToken();
            alias = ParseIdentifierName();
        }

        if (target is null && namespacePath.Count >= 1)
            target = namespacePath.Pop().Local;

        ConsumeSemicolon();

        return Finalize(node, new ImportDeclaration(NodeList.From(ref namespacePath), target!, alias));
    }

    private DelegateDeclaration ParseDelegateDeclaration()
    {
        var node = CreateNode();
        ExpectKeyword("delegate");

        var identifier = ParseIdentifierName();
        ConsumeSemicolon();

        return Finalize(node, new DelegateDeclaration(identifier, VariableDeclarationKind.Delegate));
    }

    private Statement ParseModuleDeclaration()
    {
        var node = CreateNode();

        var previousStrict = _context.Strict;
        var previousAllowSuper = _context.AllowSuperAccess;
        _context.Strict = true;

        ExpectKeyword("module");

        // ADHOC: Module Constructor

        if (Match("("))
        {
            NextToken();
            var expr = IsolateCoverGrammar(_parseExpression);
            Expect(")");

            var classBody = ParseBlock();
            _context.Strict = previousStrict;

            return Finalize(node, new ModuleConstructorStatement(expr, classBody));
        }
        else
        {

            // Regular class or module declaration
            var id = ParseVariableIdentifierAllowStatic();

            var moduleBody = ParseBlock();
            _context.Strict = previousStrict;
            _context.AllowSuperAccess = previousAllowSuper;

            return Finalize(node, new ModuleDeclaration(id, moduleBody));
        }
    }

    private Statement ParsePragmaStatement()
    {
        var node = CreateNode();
        Expect("@");

        Statement statement;
        if (MatchContextualKeyword("dump"))
        {
            NextToken();

            Expression path = ParseLeftHandSideExpression();
            statement = new PragmaDumpStatement(path);
        }
        else if (MatchContextualKeyword("exec"))
        {
            NextToken();

            BlockStatement path = ParseBlock();
            statement = new PragmaExecStatement(path);
        }
        else if (MatchContextualKeyword("current_module"))
        {
            NextToken();

            Expression path = ParseLeftHandSideExpression();
            statement = new PragmaCurrentModuleStatement(path);
        }
        else if (MatchContextualKeyword("include"))
        {
            NextToken();

            if (_lookahead.Type == TokenType.Template || _lookahead.Type == TokenType.StringLiteral)
            {
                var path = NextToken();

                statement = new PragmaIncludeStatement(new Literal((string) path.Value!, path.RawTemplate!));
            }
            else
            {
                TolerateError("Expected string literal for include statement value.", _lookahead.Value);
                statement = new PragmaIncludeStatement(null);
            }
        }
        else if (MatchKeyword("attribute") || MatchKeyword("static") || MatchKeyword("delegate") || MatchKeyword("function") ||
            MatchKeyword("method") || MatchKeyword("module") || MatchKeyword("class"))
        {
            Identifier typeName = ParseIdentifierName();

            // TODO: declarations may be paths too. support that potentially.
            var declarations = ParseVariableDeclarationList(false);
            ConsumeSemicolon();

            statement = new PragmaVarStatement(typeName, new VariableDeclaration(declarations, VariableDeclarationKind.Var));
        }
        else if (MatchContextualKeyword("no_strict"))
        {
            NextToken();

            statement = new PragmaNoStrictStatement();
        }
        else if (MatchContextualKeyword("use_strict"))
        {
            NextToken();

            statement = new PragmaUseStrictStatement();
        }
        else if (MatchContextualKeyword("push_strict"))
        {
            NextToken();

            Literal literal;
            switch (_lookahead.Type)
            {
                case TokenType.NumericLiteral:
                    {
                        _context.IsAssignmentTarget = false;
                        _context.IsBindingElement = false;
                        var token = NextToken();
                        var raw = GetTokenRaw(token);
                        literal = Finalize(node, new Literal(TokenType.StringLiteral, token.Value, raw));
                        statement = new PragmaPushStrictStatement(literal);
                    }
                    break;

                case TokenType.BooleanLiteral:
                    {
                        _context.IsAssignmentTarget = false;
                        _context.IsBindingElement = false;
                        var token = NextToken();
                        var raw = GetTokenRaw(token);
                        literal = Finalize(node, new Literal("true".Equals(token.Value), raw));
                        statement = new PragmaPushStrictStatement(literal);
                    }
                    break;

                default:
                    TolerateUnexpectedToken(_lookahead);
                    statement = new PragmaPushStrictStatement(null);
                    break;
            }
        }
        else if (MatchContextualKeyword("pop_strict"))
        {
            NextToken();

            statement = new PragmaPopStrictStatement();
        }
        else
        {
            statement = new ErrorStatement();
        }

        ConsumeSemicolon();

        return Finalize(node, statement);
    }

    private ListAssignementExpression ParseListAssignmentElementList()
    {
        Expect("|");
        var marker = CreateNode();
        var list = ParseListAssignmentElements();

        var hasRestElement = false;
        if (Match("..."))
        {
            hasRestElement = true;
            NextToken();
        }

        Expect("|");

        return Finalize(marker, new ListAssignementExpression(NodeList.From(ref list), hasRestElement));
    }

    private ListAssignmentStatement ParseListAssignment()
    {
        var node = CreateNode();
        var elements = ParseListAssignmentElementList();

        if (!Match("="))
            TolerateUnexpectedToken(_lookahead);
        NextToken();

        var init = IsolateCoverGrammar(_parseAssignmentExpression);
        ConsumeSemicolon();

        return Finalize(node, new ListAssignmentStatement(elements, init));
    }

    private ListAssignementExpression ParseListAssignmentNestedElementList()
    {
        Expect("{");
        var marker = CreateNode();
        var list = ParseListAssignmentElements();

        var hasRestElement = false;
        if (Match("..."))
        {
            hasRestElement = true;
            NextToken();
        }

        Expect("}");

        return Finalize(marker, new ListAssignementExpression(NodeList.From(ref list), hasRestElement));
    }

    private ArrayList<Node> ParseListAssignmentElements()
    {
        var list = new ArrayList<Node>();
        list.Push(ParseListAssignmentElement());

        while (Match(","))
        {
            if (IsEndOfFile())
            {
                TolerateUnexpectedToken(_lookahead);
                break;
            }

            NextToken();
            list.Push(ParseListAssignmentElement());
        }

        return list;
    }

    // ADHOC: For LIST_ASSIGN
    private Node ParseListAssignmentElement()
    {
        var node = CreateNode();
        if (MatchKeyword("var"))
        {
            NextToken();

            var id = ParseIdentifierName();
            return Finalize(node, new VariableDeclarator(id, null));
        }
        else if (Match("{"))
        {
            return ParseListAssignmentNestedElementList();
        }
        else
        {
            return ParseLeftHandSideExpression();
        }
    }

    private Expression ParseArrayOrMapInitializer()
    {
        var node = CreateNode();
        ArrayList<Expression?> arrElements = new();
        ArrayList<MapElement> map = new();

        bool isMap = false;
        Expect("[");

        // ADHOC: Added Map/KV support
        while (!Match("]"))
        {
            if (IsEndOfFile())
            {
                TolerateUnexpectedToken(_lookahead);
                break;
            }

            if (Match(","))
            {
                NextToken();
                arrElements.Add(null);
            }
            else if (Match(":"))
            {
                if (arrElements.Count > 0)
                {
                    TolerateUnexpectedToken(_lookahead, "Found mixed map and array elements in expression.");
                    break;
                }

                isMap = true;
                NextToken(); // Empty Map
            }
            else
            {
                var mark = CreateNode();
                var elem = InheritCoverGrammar(_parseAssignmentExpression);
                if (Match(":"))
                {
                    if (arrElements.Count > 0)
                    {
                        TolerateUnexpectedToken(_lookahead, "Found mixed map and array elements in expression.");
                        break;
                    }

                    isMap = true;
                    NextToken();
                    var value = InheritCoverGrammar(_parseAssignmentExpression);
                    map.Add(Finalize(mark, new MapElement(elem, value)));
                }
                else if (isMap)
                {
                    TolerateUnexpectedToken(_lookahead, "Element is a map, expected key/value.");
                    break;
                }
                else
                {
                    arrElements.Add(elem);
                }

                if (!Match("]"))
                {
                    Expect(",");
                }
            }
        }

        Expect("]");

        if (isMap)
            return Finalize(node, new MapExpression(NodeList.From(ref map)));
        else
            return Finalize(node, new ArrayExpression(NodeList.From(ref arrElements)));
    }

    private StaticDeclaration ParseStaticStatement()
    {
        var node = CreateNode();
        ExpectKeyword("static");

        var exp = ParseVariableDeclaration(false);
        ConsumeSemicolon();

        return Finalize(node, new StaticDeclaration(exp, VariableDeclarationKind.Static));
    }

    private AttributeDeclaration ParseAttributeStatement()
    {
        var node = CreateNode();
        ExpectKeyword("attribute");

        var exp = ParseAssignmentExpression();
        ConsumeSemicolon();

        return Finalize(node, new AttributeDeclaration(exp, VariableDeclarationKind.Attribute));
    }

    private FunctionExpression ParseFunctionExpression()
    {
        var node = CreateNode();

        var isAsync = MatchContextualKeyword("async");
        if (isAsync)
        {
            NextToken();
        }

        ExpectKeyword("function");

        var isGenerator = Match("*");
        if (isGenerator)
        {
            NextToken();
        }

        string? message = null;
        Expression? id = null;
        Token? firstRestricted = null;

        var previousIsAsync = _context.IsAsync;
        var previousAllowYield = _context.AllowYield;
        _context.IsAsync = isAsync;
        _context.AllowYield = !isGenerator;

        if (!Match("("))
        {
            var token = _lookahead;
            id = !_context.Strict && !isGenerator && MatchKeyword("yield")
                ? ParseIdentifierName()
                : ParseVariableIdentifier();
        }

        var formalParameters = ParseFormalParameters(firstRestricted);
        var parameters = NodeList.From(ref formalParameters.Parameters);
        var stricted = formalParameters.Stricted;
        firstRestricted = formalParameters.FirstRestricted;
        if (formalParameters.Message != null)
        {
            message = formalParameters.Message;
        }

        var previousStrict = _context.Strict;
        var previousAllowStrictDirective = _context.AllowStrictDirective;
        _context.AllowStrictDirective = formalParameters.Simple;
        var body = ParseFunctionSourceElements();

        var hasStrictDirective = _context.Strict;
        _context.Strict = previousStrict;
        _context.AllowStrictDirective = previousAllowStrictDirective;
        _context.IsAsync = previousIsAsync;
        _context.AllowYield = previousAllowYield;

        return Finalize(node, new FunctionExpression((Identifier?) id, parameters, body, isGenerator, hasStrictDirective, isAsync));
    }

    private MethodExpression ParseMethodExpression()
    {
        var node = CreateNode();

        var isAsync = MatchContextualKeyword("async");
        if (isAsync)
        {
            NextToken();
        }

        ExpectKeyword("method");

        var isGenerator = Match("*");
        if (isGenerator)
        {
            NextToken();
        }

        string? message = null;
        Expression? id = null;
        Token? firstRestricted = null;

        var previousIsAsync = _context.IsAsync;
        var previousAllowYield = _context.AllowYield;
        _context.IsAsync = isAsync;
        _context.AllowYield = !isGenerator;

        if (!Match("("))
        {
            var token = _lookahead;
            id = !_context.Strict && !isGenerator && MatchKeyword("yield")
                ? ParseIdentifierName()
                : ParseVariableIdentifier();
        }

        var formalParameters = ParseFormalParameters(firstRestricted);
        var parameters = NodeList.From(ref formalParameters.Parameters);
        var stricted = formalParameters.Stricted;
        firstRestricted = formalParameters.FirstRestricted;
        if (formalParameters.Message != null)
        {
            message = formalParameters.Message;
        }

        var previousStrict = _context.Strict;
        var previousAllowStrictDirective = _context.AllowStrictDirective;
        _context.AllowStrictDirective = formalParameters.Simple;
        var body = ParseFunctionSourceElements();

        var hasStrictDirective = _context.Strict;
        _context.Strict = previousStrict;
        _context.AllowStrictDirective = previousAllowStrictDirective;
        _context.IsAsync = previousIsAsync;
        _context.AllowYield = previousAllowYield;

        return Finalize(node, new MethodExpression((Identifier?) id, parameters, body, isGenerator, hasStrictDirective, isAsync));
    }

    private Statement ParseUndefStatement()
    {
        var node = CreateNode();
        ExpectKeyword("undef");

        if (_hasLineTerminator)
        {
            TolerateError(Messages.NewlineAfterThrow);
            return Finalize(node, new ErrorStatement());
        }

        var expr = ParseVariableIdentifierAllowStatic();
        ConsumeSemicolon();

        return Finalize(node, new UndefStatement(expr));
    }

    // ADHOC: Object finalizer
    private FinalizerStatement ParseFinalizer()
    {
        var node = CreateNode();
        ExpectKeyword("finally");

        var block = ParseBlock();
        return Finalize(node, new FinalizerStatement(block));
    }

    // ADHOC: self object finalizer
    private SelfFinalizerExpression ParseSelfFinalizer()
    {
        var node = CreateNode();
        ExpectKeyword("finally");

        var block = ParseBlock();
        return Finalize(node, new SelfFinalizerExpression(block));
    }

    // ADHOC: Print Statement
    private PrintStatement ParsePrintStatement()
    {
        var node = CreateNode();
        ExpectKeyword("print");

        List<Expression> expressions = new List<Expression>();
        do
        {
            if (IsEndOfFile())
            {
                TolerateUnexpectedToken(_lookahead);
                break;
            }

            var exp = ParseExpression();
            expressions.Add(exp);
        }
        while (Match(","));

        ConsumeSemicolon();

        return Finalize(node, new PrintStatement(NodeList.Create(expressions)));
    }

    private RequireStatement ParseRequireStatement()
    {
        var node = CreateNode();
        NextToken();

        Expression expr = ParseExpression();
        return Finalize(node, new RequireStatement(expr));
    }

    private Statement ParsePreprocessorDirectiveStatement()
    {
        var node = CreateNode();
        Expect("#");

        if (_lookahead.Type == TokenType.NumericLiteral)
        {
            int line = (int) _lookahead.Value!;
            NextToken();

            // We need to set this before parsing the source string, since _lookahead will be set in NextToken().
            _scanner._lineNumber = line - 1;
            _startMarker = new Marker(_startMarker.Index, line - 1, _startMarker.Column);
            _lastMarker = new Marker(_lastMarker.Index, line - 1, _lastMarker.Column);

            var fileToken = NextToken();
            SetFileName(fileToken.RawTemplate!);

            return new SourceFileStatement(new Literal(fileToken.RawTemplate!, fileToken.RawTemplate!));
        }
        else
        {
            TolerateUnexpectedToken(_lookahead, "Unexpected token for preprocessor directive statement");
        }

        return Finalize(node, new ErrorStatement());
    }
    #endregion

    private void SetFileName(string fileName)
    {
        _scanner._sourceLocation = fileName;
    }

    private ParseError CreateError(string messageFormat, params object?[] values)
    {
        var msg = string.Format(CultureInfo.InvariantCulture, messageFormat, values);

        var index = _lastMarker.Index;
        var line = _lastMarker.Line;
        var column = _lastMarker.Column;
        return _errorHandler.CreateError(_scanner._sourceLocation, index, line, column, msg);
    }

    private protected void TolerateError(string messageFormat, params object?[] values)
    {
        var msg = string.Format(CultureInfo.InvariantCulture, messageFormat, values);

        var index = _lastMarker.Index;
        var line = _scanner._lineNumber;
        var column = _lastMarker.Column;
        _errorHandler.TolerateError(_scanner._sourceLocation, index, line, column, msg, _tolerant);
    }

    private ParseError UnexpectedTokenError(in Token token, string? message = null)
    {
        var msg = message ?? Messages.UnexpectedToken;
        string value;

        if (token.Type != TokenType.Unknown)
        {
            if (message == null)
            {
                msg = token.Type == TokenType.EOF ? Messages.UnexpectedEOS :
                    token.Type == TokenType.Identifier ? Messages.UnexpectedIdentifier :
                    token.Type == TokenType.NumericLiteral ? Messages.UnexpectedNumber :
                    token.Type == TokenType.StringLiteral ? Messages.UnexpectedString :
                    token.Type == TokenType.Template ? Messages.UnexpectedTemplate :
                    Messages.UnexpectedToken;
            }

            value = token.Type == TokenType.Template
                ? token.RawTemplate!
                : Convert.ToString(token.Value, CultureInfo.InvariantCulture);
        }
        else
        {
            value = "ILLEGAL";
        }

        msg = string.Format(CultureInfo.InvariantCulture, msg, value);

        if (token.Type != TokenType.Unknown && token.LineNumber > 0)
        {
            var index = token.Start;
            var line = token.LineNumber;
            var lastMarkerLineStart = _lastMarker.Index - _lastMarker.Column;
            var column = token.Start - lastMarkerLineStart;
            return _errorHandler.CreateError(_scanner._sourceLocation, index, line, column, msg);
        }
        else
        {
            var index = _lastMarker.Index;
            var line = _lastMarker.Line;
            var column = _lastMarker.Column;
            return _errorHandler.CreateError(_scanner._sourceLocation, index, line, column, msg);
        }
    }

    private protected void TolerateUnexpectedToken(in Token token, string? message = null)
    {
        _errorHandler.Tolerate(UnexpectedTokenError(token, message), _tolerant);
    }

    private void TolerateInvalidLoopStatement()
    {
        if (MatchKeyword("class") || MatchKeyword("function"))
        {
            TolerateUnexpectedToken(_lookahead);
        }
    }

    private struct ParsedParameters
    {
        private HashSet<string?>? paramSet;
        public Token? FirstRestricted;
        public string? Message;
        public ArrayList<Expression> Parameters = new();
        public Token? Stricted;
        public bool Simple;
        public bool HasDuplicateParameterNames;

        public ParsedParameters()
        {
            paramSet = null;
            FirstRestricted = null;
            Message = null;
            Stricted = null;
            Simple = false;
            HasDuplicateParameterNames = false;
        }

        public readonly bool ParamSetContains(string? key)
        {
            return paramSet != null && paramSet.Contains(key);
        }

        public void ParamSetAdd(string? key)
        {
            (paramSet ??= new HashSet<string?>()).Add(key);
        }
    }
}
