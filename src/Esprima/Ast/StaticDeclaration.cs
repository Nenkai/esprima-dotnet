using Esprima.Utils;

namespace Esprima.Ast
{
    public sealed class StaticDeclaration : Declaration
    {
        public VariableDeclaration Declaration;

        public override NodeCollection ChildNodes => new NodeCollection(Declaration);

        public StaticDeclaration(
            VariableDeclaration expr)
            : base(Nodes.StaticDeclaration)
        {
            Declaration = expr;
        }

        protected internal override void Accept(AstVisitor visitor)
        {
            
        }
    }
}
