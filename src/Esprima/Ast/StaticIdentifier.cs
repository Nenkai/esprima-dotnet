using Esprima.Utils;

namespace Esprima.Ast
{
    public sealed class StaticIdentifier : Expression
    {
        public Identifier Id { get; set; }

        public StaticIdentifier(Identifier identifier) : base(Nodes.StaticIdentifier)
        {
            Id = identifier;
        }

        public override NodeCollection ChildNodes => NodeCollection.Empty;

        protected internal override void Accept(AstVisitor visitor)
        {
            
        }
    }
}
