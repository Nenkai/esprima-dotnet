using System;
using Esprima.Utils;

namespace Esprima.Ast
{
    public class PragmaIncludeStatement : Statement
    {
        public string Path { get; set; }

        public PragmaIncludeStatement(string path) : base(Nodes.PragmaIncludeStatement)
        {
        }

        public override NodeCollection ChildNodes => NodeCollection.Empty;

        protected internal override void Accept(AstVisitor visitor)
        {
            throw new NotImplementedException();
        }
    }
}
