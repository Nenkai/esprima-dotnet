using System.Xml.Linq;
using Esprima.Utils;

namespace Esprima.Ast
{
    public class DelegateDeclaration : Declaration
    {
        public readonly VariableDeclarationKind Kind;
        public Identifier Identifier { get; set; }

        public DelegateDeclaration(Identifier identifier, VariableDeclarationKind kind)
            : base(Nodes.DelegateDeclaration)
        {
            Kind = kind;
            Identifier = identifier;
        }

        public override NodeCollection ChildNodes => new NodeCollection(Identifier);

        protected internal override void Accept(AstVisitor visitor)
        {

        }
    }
}
