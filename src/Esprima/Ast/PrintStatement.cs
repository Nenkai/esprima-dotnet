using Esprima.Utils;
using System.Collections.Generic;

namespace Esprima.Ast
{
    // Adhoc
    public class PrintStatement : Statement
    {
        public List<Expression> Expressions { get; }

        public PrintStatement(List<Expression> expression)
            : base(Nodes.PrintStatement)
        {
            Expressions = expression;
        }

        public override NodeCollection ChildNodes => new();

        protected internal override void Accept(AstVisitor visitor)
        {

        }
    }

}
