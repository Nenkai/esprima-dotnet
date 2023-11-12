using Esprima.Utils;

namespace Esprima.Ast
{
    public sealed class StaticDeclaration : Declaration
    {
        public readonly VariableDeclarationKind Kind;

        public VariableDeclarator Declaration;

        public override NodeCollection ChildNodes => new NodeCollection(Declaration);

        public StaticDeclaration(
            VariableDeclarator expr,
            VariableDeclarationKind kind)
            : base(Nodes.StaticDeclaration)
        {
            Declaration = expr;
            Kind = kind;
        }

        protected internal override void Accept(AstVisitor visitor)
        {
            
        }
    }
}
