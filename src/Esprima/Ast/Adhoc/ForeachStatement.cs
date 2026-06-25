using System.Runtime.CompilerServices;
using Esprima.Utils;

namespace Esprima.Ast;

[VisitableNode(ChildProperties = new[] { nameof(Left), nameof(Right), nameof(Body) })]
public sealed partial class ForeachStatement : Statement
{
    public ForeachStatement(
        Node left,
        Expression right,
        Statement body) : base(Nodes.ForeachStatement)
    {
        Left = left;
        Right = right;
        Body = body;
    }

    public Node Left { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }
    public Expression Right { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }
    public Statement Body { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static ForeachStatement Rewrite(Node left, Expression right, Statement body)
    {
        return new ForeachStatement(left, right, body);
    }
}
