using System.Runtime.CompilerServices;

namespace Esprima.Ast;

[VisitableNode(ChildProperties = new[] { nameof(Block), nameof(Handler) })]
public sealed partial class TryStatement : Statement
{
    public TryStatement(
        BlockStatement block,
        CatchClause? handler) :
        base(Nodes.TryStatement)
    {
        Block = block;
        Handler = handler;
    }

    public BlockStatement Block { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }
    public CatchClause? Handler { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static TryStatement Rewrite(BlockStatement block, CatchClause? handler)
    {
        return new TryStatement(block, handler);
    }
}
