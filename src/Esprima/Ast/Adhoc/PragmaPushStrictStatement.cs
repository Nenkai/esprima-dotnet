using System;
using System.Runtime.CompilerServices;
using Esprima.Utils;

namespace Esprima.Ast;

[VisitableNode(ChildProperties = new[] { nameof(Value) })]
public sealed partial class PragmaPushStrictStatement : Statement
{
    public PragmaPushStrictStatement(Literal? value) : base(Nodes.PragmaPushStrictStatement)
    {
        Value = value;
    }

    public Literal? Value { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; } // Literal or bool

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static PragmaPushStrictStatement Rewrite(Literal? literal)
    {
        return new PragmaPushStrictStatement(literal);
    }
}
