using System.Runtime.CompilerServices;

namespace Esprima.Ast;

[VisitableNode(ChildProperties = new[] { nameof(Id), nameof(Init) })]
public sealed partial class VariableDeclarator : Node
{
    public VariableDeclarator(Identifier id, Expression? init) :
        base(Nodes.VariableDeclarator)
    {
        Id = id;
        Init = init;
    }

    public Identifier Id { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }
    public Expression? Init { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static VariableDeclarator Rewrite(Identifier id, Expression? init)
    {
        return new VariableDeclarator(id, init);
    }
}
