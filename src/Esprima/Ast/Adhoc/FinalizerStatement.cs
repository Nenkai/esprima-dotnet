using System;
using System.Runtime.CompilerServices;
using Esprima.Utils;

namespace Esprima.Ast;

[VisitableNode(ChildProperties = new[] { nameof(Body) })]
public sealed partial class FinalizerStatement : Statement
{
    public FinalizerStatement(Statement body)
        : base(Nodes.FinalizerStatement)
    {
        Body = body;
    }

    public Statement Body { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static FinalizerStatement Rewrite(Statement body)
    {
        return new FinalizerStatement(body);
    }
}
