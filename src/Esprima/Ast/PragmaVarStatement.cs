using System;
using Esprima.Utils;

namespace Esprima.Ast
{
    public class PragmaVarStatement : Statement
    {
        public Identifier Type { get; set; }
        public VariableDeclaration Declaration { get; set; }

        public PragmaVarStatement(Identifier typeName, VariableDeclaration declaration) : base(Nodes.PragmaVarStatement)
        {
            Type = typeName;
            Declaration = declaration;
        }

        public override NodeCollection ChildNodes => NodeCollection.Empty;

        protected internal override void Accept(AstVisitor visitor)
        {
            throw new NotImplementedException();
        }
    }
}
