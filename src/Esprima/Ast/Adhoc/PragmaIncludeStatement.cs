using System;
using System.Runtime.CompilerServices;
using Esprima.Utils;

namespace Esprima.Ast;

[VisitableNode(ChildProperties = new[] { nameof(Path) })]
public sealed partial class PragmaIncludeStatement : Statement
{
    public PragmaIncludeStatement(Literal? literal) : base(Nodes.PragmaIncludeStatement)
    {
    }

    public Literal? Path { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static PragmaIncludeStatement Rewrite(Literal? literal)
    {
        return new PragmaIncludeStatement(literal);
    }
}
