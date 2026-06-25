using System.Numerics;
using System.Runtime.CompilerServices;
using System.Text.RegularExpressions;

namespace Esprima.Ast;

[VisitableNode(SealOverrideMethods = true)]
public partial class Literal : Expression
{
    internal Literal(TokenType tokenType, object? value, string raw) : base(Nodes.Literal)
    {
        TokenType = tokenType;
        Value = value;
        Raw = raw;
    }

    public Literal(string value, string raw) : this(TokenType.StringLiteral, value, raw)
    {
    }

    public Literal(bool value, string raw) : this(TokenType.BooleanLiteral, value.AsCachedObject(), raw)
    {
    }

    public Literal(NumericTokenType numericTokenType, object value, string raw) : this(TokenType.NumericLiteral, value, raw)
    {
        NumericTokenType = numericTokenType;
    }

    public Literal(string raw) : this(TokenType.NilLiteral, null, raw)
    {
    }

    public TokenType TokenType { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }
    public object? Value { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }
    public string Raw { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }
    public NumericTokenType NumericTokenType { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }

    public string? StringValue { [MethodImpl(MethodImplOptions.AggressiveInlining)] get => TokenType == TokenType.StringLiteral ? (string) Value! : null; }
    public bool? BooleanValue { [MethodImpl(MethodImplOptions.AggressiveInlining)] get => TokenType == TokenType.BooleanLiteral ? ReferenceEquals(Value, ParserExtensions.s_boxedTrue) : null; }
    public object? NumericValue { [MethodImpl(MethodImplOptions.AggressiveInlining)] get => TokenType == TokenType.NumericLiteral ? Value! : null; }
}
