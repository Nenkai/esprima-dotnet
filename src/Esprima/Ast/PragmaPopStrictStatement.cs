using System;
using Esprima.Utils;

namespace Esprima.Ast
{
    public class PragmaPopStrictStatement : Statement
    {
        public PragmaPopStrictStatement() : base(Nodes.PragmaPopStrictStatement)
        {

        }

        public override NodeCollection ChildNodes => NodeCollection.Empty;

        protected internal override void Accept(AstVisitor visitor)
        {
            throw new NotImplementedException();
        }
    }
}
