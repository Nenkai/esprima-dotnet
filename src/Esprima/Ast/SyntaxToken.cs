using System.Runtime.CompilerServices;

namespace Esprima.Ast;

public class SyntaxToken : SyntaxElement
{
    public SyntaxToken(TokenType type, string value)
    {
        Type = type;

        Value = value;
    }

    public TokenType Type { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }

    public string Value { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }


    public override string ToString() => Value;
}
