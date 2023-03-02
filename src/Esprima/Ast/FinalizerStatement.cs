using Esprima.Utils;

namespace Esprima.Ast
{
    public class FinalizerStatement : Statement
    {
        public Statement Body { get; }

        public FinalizerStatement(Statement body)
            : base(Nodes.FinalizerStatement)
        {
            Body = body;
        }

        public override NodeCollection ChildNodes => new();

        protected internal override void Accept(AstVisitor visitor)
        {

        }
    }

}
