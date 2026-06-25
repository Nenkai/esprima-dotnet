using System;
using System.Runtime.CompilerServices;
using System.Xml.Linq;
using Esprima.Utils;

namespace Esprima.Ast;

[VisitableNode(ChildProperties = new[] { nameof(Declaration) })]
public sealed partial class StaticDeclaration : Declaration
{
    public StaticDeclaration(
        VariableDeclarator expr,
        VariableDeclarationKind kind)
        : base(Nodes.StaticDeclaration)
    {
        Declaration = expr;
        Kind = kind;
    }

    public readonly VariableDeclarationKind Kind;
    public VariableDeclarator Declaration { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private StaticDeclaration Rewrite(VariableDeclarator declaration)
    {
        return new StaticDeclaration(declaration, Kind);
    }
}
