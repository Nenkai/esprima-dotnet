using System;
using Esprima.Utils;

namespace Esprima.Ast
{
    public class PragmaDumpStatement : Statement
    {
        public Expression Path { get; set; }

        public PragmaDumpStatement(Expression pathExpression) : base(Nodes.PragmaDumpStatement)
        {
            Path = pathExpression;
        }

        public override NodeCollection ChildNodes => NodeCollection.Empty;

        protected internal override void Accept(AstVisitor visitor)
        {
            throw new NotImplementedException();
        }
    }
}
