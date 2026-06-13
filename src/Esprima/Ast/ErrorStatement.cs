using System;
using Esprima.Utils;

namespace Esprima.Ast
{
    public sealed class ErrorStatement : Statement
    {
        public ErrorStatement() : base(Nodes.ErrorStatement)
        {

        }

        public override NodeCollection ChildNodes => new();

        protected internal override void Accept(AstVisitor visitor)
        {
            throw new NotImplementedException();
        }
    }
}
