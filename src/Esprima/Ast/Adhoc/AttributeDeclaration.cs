using System;
using System.Runtime.CompilerServices;
using System.Xml.Linq;
using Esprima.Utils;

namespace Esprima.Ast;

[VisitableNode(ChildProperties = new[] { nameof(VarExpression) })]
public sealed partial class AttributeDeclaration : Declaration
{
    public AttributeDeclaration(
        Expression expr,
        VariableDeclarationKind kind)
        : base(Nodes.AttributeDeclaration)
    {
        VarExpression = expr;
        Kind = kind;
    }

    public Expression VarExpression { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }
    public readonly VariableDeclarationKind Kind;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private AttributeDeclaration Rewrite(Expression varExpression)
    {
        return new AttributeDeclaration(varExpression, Kind);
    }
}
