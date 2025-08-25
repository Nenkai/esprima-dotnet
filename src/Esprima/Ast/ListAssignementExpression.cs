using System;
using Esprima.Utils;

namespace Esprima.Ast
{
    /// <summary>
    /// List assignment declaration, without init value
    /// </summary>
    public sealed class ListAssignementExpression : Expression
    {
        private readonly NodeList<Node> _declarations;
        public readonly bool HasRestElement;

        public ListAssignementExpression(
            in NodeList<Node> declarations,
            bool hasRestElement)
            : base(Nodes.ListAssignmentExpression)
        {
            _declarations = declarations;
            HasRestElement = hasRestElement;
        }

        public ref readonly NodeList<Node> Elements => ref _declarations;

        public override NodeCollection ChildNodes => GenericChildNodeYield.Yield(_declarations);

        protected internal override void Accept(AstVisitor visitor)
        {
            throw new NotImplementedException();
        }
    }
}
