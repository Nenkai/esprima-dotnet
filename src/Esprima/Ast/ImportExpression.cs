﻿using Esprima.Utils;

namespace Esprima.Ast
{
    public class ImportExpression : Expression
    {
        public ImportDeclaration Declaration { get; set; }

        public ImportExpression(Nodes node, Location location)
            : base(node)
        {
            Location = location;
        }

        public override NodeCollection ChildNodes => new NodeCollection(Declaration);

        protected internal override void Accept(AstVisitor visitor)
        {

        }
    }
}