﻿using Esprima.Utils;

namespace Esprima.Ast
{
    public class ErrorExpression : Expression
    {
        public ErrorExpression() : base(Nodes.ExpressionStatement)
        {
            
        }

        public override NodeCollection ChildNodes => new();

        protected internal override void Accept(AstVisitor visitor)
        {

        }
    }
}
