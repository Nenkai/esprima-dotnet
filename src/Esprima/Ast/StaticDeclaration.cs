using Esprima.Utils;

namespace Esprima.Ast
{
    public sealed class StaticDeclaration : Declaration
    {
        public readonly VariableDeclarationKind Kind;

        public Expression Expression;

        public override NodeCollection ChildNodes => new NodeCollection(Expression);

        public StaticDeclaration(
            Expression expr,
            VariableDeclarationKind kind)
            : base(Nodes.StaticDeclaration)
        {
            Expression = expr;
            Kind = kind;
        }

        protected internal override void Accept(AstVisitor visitor)
        {
            
        }
    }
}
