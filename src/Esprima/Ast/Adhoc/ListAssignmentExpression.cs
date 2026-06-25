using System;
using System.Runtime.CompilerServices;
using Esprima.Utils;

namespace Esprima.Ast;

/// <summary>
/// List assignment declaration, without init value
/// </summary>
[VisitableNode(ChildProperties = new[] { nameof(Declarations) })]
public sealed partial class ListAssignementExpression : Expression
{
    private readonly NodeList<Node> _declarations;
    public readonly bool HasRestElement;

    public ListAssignementExpression(
        in NodeList<Node> declarations,
        bool hasRestElement)
        : base(Nodes.ListAssignmentExpression)
    {
        _declarations = declarations;
        HasRestElement = hasRestElement;
    }

    public ref readonly NodeList<Node> Declarations => ref _declarations;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private ListAssignementExpression Rewrite(in NodeList<Node> declarations)
    {
        return new ListAssignementExpression(declarations, HasRestElement);
    }
}
