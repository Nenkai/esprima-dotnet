using System.Numerics;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using Esprima.Ast;

namespace Esprima;

public enum TokenType : byte
{
    Unknown,
    BooleanLiteral,
    EOF,
    Identifier,
    Keyword,
    NilLiteral,
    NumericLiteral,
    Punctuator,
    StringLiteral,
    Template,
    SymbolLiteral, // ADHOC

    Extension = byte.MaxValue
}

public enum NumericTokenType
{
    None,
    Integer,
    UnsignedInteger,
    Long,
    UnsignedLong,
    Float,
    Double,
}

public enum LegacyOctalKind : byte
{
    None,
    Octal,
    Escaped8or9,
}

[StructLayout(LayoutKind.Auto)]
public readonly record struct Token
{
    public abstract record ValueHolder(object? Value);

    internal readonly object? _value;

    internal Token(
        TokenType type,
        object? value,
        NumericTokenType numericTokenType,
        int start,
        int end,
        int lineNumber,
        int lineStart,
        LegacyOctalKind octalKind = LegacyOctalKind.None,
        bool hasHexEscape = false)
    {
        Type = type;
        NumericTokenType = numericTokenType;
        _value = value;

        OctalKind = octalKind;
        Start = start;
        End = end;
        LineNumber = lineNumber;
        LineStart = lineStart;
        HasHexEscape = hasHexEscape;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Token Create(TokenType type, object? value, int start, int end, int lineNumber, int lineStart)
    {
        return new Token(type, value, NumericTokenType.None, start, end, lineNumber, lineStart);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Token CreateStringLiteral(string str, LegacyOctalKind octalKind, int start, int end, int lineNumber, int lineStart)
    {
        return new Token(TokenType.StringLiteral, str, NumericTokenType.None, start, end, lineNumber, lineStart, octalKind);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Token CreateNumericLiteral(object value, NumericTokenType numericTokenType, bool octal, int start, int end, int lineNumber, int lineStart)
    {
        return new Token(TokenType.NumericLiteral, value, numericTokenType, start, end, lineNumber, lineStart, octal ? LegacyOctalKind.Octal : LegacyOctalKind.None);
    }

    public static Token CreateEof(int index, int lineNumber, int lineStart)
    {
        return new Token(TokenType.EOF, value: null, NumericTokenType.None, start: index, end: index, lineNumber, lineStart);
    }

    public static Token CreatePunctuator(string str, int start, int end, int lineNumber, int lineStart)
    {
        return new Token(TokenType.Punctuator, str, NumericTokenType.None, start, end, lineNumber, lineStart);
    }

    public sealed record TemplateHolder(object? Value, string RawTemplate, char NotEscapeSequenceHead, bool Head, bool Tail) : ValueHolder(Value);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Token CreateTemplate(
        string? cooked,
        string rawTemplate,
        bool head,
        bool tail,
        char notEscapeSequenceHead,
        bool hasHexEscape,
        int start,
        int end,
        int lineNumber,
        int lineStart)
    {
        var value = new TemplateHolder(cooked, rawTemplate, notEscapeSequenceHead, head, tail);
        return new Token(TokenType.Template, value, NumericTokenType.None, start, end, lineNumber, lineStart, hasHexEscape: hasHexEscape);
    }

    public readonly TokenType Type;
    public readonly NumericTokenType NumericTokenType;
    internal readonly LegacyOctalKind OctalKind;
    public readonly bool Octal => OctalKind == LegacyOctalKind.Octal;

    public readonly int Start; // Range[0]
    public readonly int End; // Range[1]
    public readonly int LineNumber;
    public readonly int LineStart;
    public readonly bool HasHexEscape;

    public object? Value
    {
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        get
        {
            // NOTE: This condition must not be inverted, otherwise the runtime (.NET 6) fail to inline the accessor correctly.
            return Type is not (TokenType.Template or TokenType.Extension)
                ? _value
                : GetValueFromHolder(in this);

            [MethodImpl(MethodImplOptions.NoInlining)]
            static object? GetValueFromHolder(in Token token) => ((ValueHolder) token._value!).Value;
        }
    }

    internal char NotEscapeSequenceHead => Type == TokenType.Template ? ((TemplateHolder) _value!).NotEscapeSequenceHead : char.MinValue;
    public bool Head => Type == TokenType.Template && ((TemplateHolder) _value!).Head;
    public bool Tail => Type == TokenType.Template && ((TemplateHolder) _value!).Tail;

    public string? RawTemplate
    {
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        get => Type == TokenType.Template ? ((TemplateHolder) _value!).RawTemplate : null;
    }

    public object? GetRaw() => _value;

    internal Token ChangeType(TokenType newType)
    {
        return new Token(newType, _value, NumericTokenType, Start, End, LineNumber, LineStart, OctalKind);
    }

    internal bool IsEscaped(string value) => value.Length != End - Start;
}
