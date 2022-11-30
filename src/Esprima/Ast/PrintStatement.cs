using Esprima.Utils;

namespace Esprima.Ast
{
    // Adhoc
    public class PrintStatement : Statement
    {
        public Expression Expression { get; }

        public PrintStatement(Expression expression)
            : base(Nodes.PrintStatement)
        {
            Expression = expression;
        }

        public override NodeCollection ChildNodes => new();

        protected internal override void Accept(AstVisitor visitor)
        {

        }
    }

}
