﻿using Esprima.Utils;

namespace Esprima.Ast
{
    public sealed class Super : Expression
    {
        public Super() : base(Nodes.Super)
        {
        }

        public override NodeCollection ChildNodes => NodeCollection.Empty;

        protected internal override void Accept(AstVisitor visitor)
        {
            visitor.VisitSuper(this);
        }
    }
}
