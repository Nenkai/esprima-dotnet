using System;
using System.Runtime.CompilerServices;
using System.Xml.Linq;
using Esprima.Utils;

namespace Esprima.Ast;

[VisitableNode(ChildProperties = new[] { nameof(Identifier) })]
public sealed partial class DelegateDeclaration : Declaration
{
    public DelegateDeclaration(Identifier identifier, VariableDeclarationKind kind)
        : base(Nodes.DelegateDeclaration)
    {
        Kind = kind;
        Identifier = identifier;
    }

    public Identifier Identifier { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }
    public readonly VariableDeclarationKind Kind;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private DelegateDeclaration Rewrite(Identifier id)
    {
        return new DelegateDeclaration(id, Kind);
    }
}
