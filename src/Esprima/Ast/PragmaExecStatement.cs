using System;
using Esprima.Utils;

namespace Esprima.Ast
{
    public class PragmaExecStatement : Statement
    {
        public BlockStatement Statements { get; set; }

        public PragmaExecStatement(BlockStatement blockStatement) : base(Nodes.PragmaExecStatement)
        {
            Statements = blockStatement;
        }

        public override NodeCollection ChildNodes => NodeCollection.Empty;

        protected internal override void Accept(AstVisitor visitor)
        {
            throw new NotImplementedException();
        }
    }
}
