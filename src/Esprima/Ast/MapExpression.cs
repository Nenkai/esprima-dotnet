using System;
using System.Collections.Generic;
using Esprima.Utils;

namespace Esprima.Ast
{
    // Map/Dictionary for adhoc
    public sealed class MapExpression : Expression
    {
        private readonly Dictionary<Expression, Expression> _elements;

        public MapExpression(in Dictionary<Expression, Expression> elements) : base(Nodes.MapExpression)
        {
            _elements = elements;
        }

        public ref readonly Dictionary<Expression, Expression> Elements => ref _elements;

        public override NodeCollection ChildNodes => throw new System.Exception("Not implemented");


        protected internal override void Accept(AstVisitor visitor)
        {
            throw new NotImplementedException();
        }
    }
}
