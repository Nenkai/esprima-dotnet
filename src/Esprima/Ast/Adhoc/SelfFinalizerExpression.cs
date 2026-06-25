using System;
using System.Reflection.Emit;
using System.Runtime.CompilerServices;
using Esprima.Utils;

namespace Esprima.Ast;

[VisitableNode(ChildProperties = new[] { nameof(Body) })]
public sealed partial class SelfFinalizerExpression : Expression
{
    public SelfFinalizerExpression(Statement body)
        : base(Nodes.SelfFinalizerExpression)
    {
        Body = body;
    }

    public Statement Body { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static SelfFinalizerExpression Rewrite(Statement body)
    {
        return new SelfFinalizerExpression(body);
    }
}
