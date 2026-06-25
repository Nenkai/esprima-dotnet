using System;
using System.Runtime.CompilerServices;
using Esprima.Utils;

namespace Esprima.Ast;

[VisitableNode(ChildProperties = new[] { nameof(Left), nameof(Right) })]
public sealed partial class ListAssignmentStatement : Declaration
{
    public ListAssignmentStatement(ListAssignementExpression specifiers, Expression init)
        : base(Nodes.ListAssignmentStatement)
    {
        Left = specifiers;
        Right = init;
    }

    /// <summary>
    /// Variable list
    /// </summary>
    public ListAssignementExpression Left { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }

    /// <summary>
    /// Init expression
    /// </summary>
    public Expression Right { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static ListAssignmentStatement Rewrite(ListAssignementExpression left, Expression right)
    {
        return new ListAssignmentStatement(left, right);
    }
}
