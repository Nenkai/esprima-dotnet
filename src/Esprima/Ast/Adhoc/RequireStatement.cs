using System;
using System.Runtime.CompilerServices;
using System.Xml.Linq;
using Esprima.Utils;

namespace Esprima.Ast;

[VisitableNode(ChildProperties = new[] { nameof(Expression) })]
public sealed partial class RequireStatement : Statement
{
    public RequireStatement(Expression expression) : base(Nodes.RequireStatement)
    {
        Expression = expression;
    }

    public Expression Expression { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static RequireStatement Rewrite(Expression expression)
    {
        return new RequireStatement(expression);
    }
}
