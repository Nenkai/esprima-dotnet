using System;
using Esprima.Utils;

namespace Esprima.Ast
{
    public sealed class ListAssignementStatement : Declaration
    {
        /// <summary>
        /// Variable list
        /// </summary>
        public readonly ListAssignementExpression Left;

        /// <summary>
        /// Init expression
        /// </summary>
        public readonly Expression Right;

        public ListAssignementStatement(
            ListAssignementExpression specifiers,
            Expression init)
            : base(Nodes.ListAssignmentStatement)
        {
            Left = specifiers;
            Right = init;
        }

        public override NodeCollection ChildNodes => new(Left, Right);

        protected internal override void Accept(AstVisitor visitor)
        {
            throw new NotImplementedException();
        }
    }
}
