using System;
using Esprima.Utils;

namespace Esprima.Ast
{
    public class PragmaUseStrictStatement : Statement
    {
        public PragmaUseStrictStatement() : base(Nodes.PragmaUseStrictStatement)
        {

        }

        public override NodeCollection ChildNodes => NodeCollection.Empty;

        protected internal override void Accept(AstVisitor visitor)
        {
            throw new NotImplementedException();
        }
    }
}
