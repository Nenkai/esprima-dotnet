using System;
using Esprima.Utils;

namespace Esprima.Ast
{
    public class PragmaCurrentModuleStatement : Statement
    {
        public Expression Path { get; set; }

        public PragmaCurrentModuleStatement(Expression path) : base(Nodes.PragmaCurrentModuleStatement)
        {
            Path = path;
        }

        public override NodeCollection ChildNodes => NodeCollection.Empty;

        protected internal override void Accept(AstVisitor visitor)
        {
            throw new NotImplementedException();
        }
    }
}
