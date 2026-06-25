using System;
using System.Runtime.CompilerServices;
using Esprima.Utils;

namespace Esprima.Ast;

[VisitableNode(ChildProperties = new[] { nameof(Path) })]
public sealed partial class PragmaDumpStatement : Statement
{
    public PragmaDumpStatement(Expression pathExpression) : base(Nodes.PragmaDumpStatement)
    {
        Path = pathExpression;
    }

    public Expression Path { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static PragmaDumpStatement Rewrite(Expression path)
    {
        return new PragmaDumpStatement(path);
    }
}
