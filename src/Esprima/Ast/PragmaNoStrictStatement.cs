using System;
using Esprima.Utils;

namespace Esprima.Ast
{
    public class PragmaNoStrictStatement : Statement
    {
        public PragmaNoStrictStatement() : base(Nodes.PragmaNoStrictStatement)
        {

        }

        public override NodeCollection ChildNodes => NodeCollection.Empty;

        protected internal override void Accept(AstVisitor visitor)
        {
            throw new NotImplementedException();
        }
    }
}
