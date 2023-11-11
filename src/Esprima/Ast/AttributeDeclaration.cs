using Esprima.Utils;

namespace Esprima.Ast
{
    public sealed class AttributeDeclaration : Declaration
    {
        public readonly VariableDeclarationKind Kind;

        public Expression VarExpression;

        public override NodeCollection ChildNodes => new NodeCollection(VarExpression);

        public AttributeDeclaration(
            Expression expr,
            VariableDeclarationKind kind)
            : base(Nodes.AttributeDeclaration)
        {
            VarExpression = expr;
            Kind = kind;
        }

        protected internal override void Accept(AstVisitor visitor)
        {
            
        }
    }
}
