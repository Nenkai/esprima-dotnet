using System.Xml.Linq;
using Esprima.Utils;

namespace Esprima.Ast
{
    public class DelegateDefinition : Statement
    {
        public Identifier Identifier { get; set; }

        public DelegateDefinition(Identifier identifier)
            : base(Nodes.DelegateDefinition)
        {
            Identifier = identifier;
        }

        public override NodeCollection ChildNodes => new NodeCollection(Identifier);

        protected internal override void Accept(AstVisitor visitor)
        {

        }
    }
}
