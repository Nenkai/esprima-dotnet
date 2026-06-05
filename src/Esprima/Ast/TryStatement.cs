using Esprima.Utils;

namespace Esprima.Ast
{
    public sealed class TryStatement : Statement
    {
        public readonly Statement Block;
        public readonly CatchClause? Handler;

        public TryStatement(
            Statement block,
            CatchClause? handler) :
            base(Nodes.TryStatement)
        {
            Block = block;
            Handler = handler;
        }

        public override NodeCollection ChildNodes => new(Block, Handler);

        protected internal override void Accept(AstVisitor visitor)
        {
            visitor.VisitTryStatement(this);
        }
    }
}
