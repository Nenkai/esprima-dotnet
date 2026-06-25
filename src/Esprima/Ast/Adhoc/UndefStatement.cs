using System;
using System.Runtime.CompilerServices;
using System.Xml.Linq;
using Esprima.Utils;

namespace Esprima.Ast;

[VisitableNode(ChildProperties = new[] { nameof(Expression) })]
public sealed partial class UndefStatement : Statement
{
    public UndefStatement(Expression expression) : base(Nodes.UndefStatement)
    {
        Expression = expression;
    }

    public Expression Expression { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static UndefStatement Rewrite(Expression expression)
    {
        return new UndefStatement(expression);
    }
}
