using Esprima.Utils;

namespace Esprima.Ast
{
    public class SelfFinalizerExpression : Expression
    {
        public Statement Body { get; }

        public SelfFinalizerExpression(Statement body)
            : base(Nodes.SelfFinalizerExpression)
        {
            Body = body;
        }

        public override NodeCollection ChildNodes => new();

        protected internal override void Accept(AstVisitor visitor)
        {

        }
    }

}
