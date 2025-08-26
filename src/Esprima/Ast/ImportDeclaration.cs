using Esprima.Utils;

namespace Esprima.Ast
{
    public sealed class ImportDeclaration : Declaration
    {
        private readonly NodeList<ImportDeclarationSpecifier> _specifiers;

        public Identifier Target { get; set; }
        public Identifier Alias { get; set; }

        public ImportDeclaration(
            in NodeList<ImportDeclarationSpecifier> namespacePath, Identifier target, Identifier? alias)
            : base(Nodes.ImportDeclaration)
        {
            _specifiers = namespacePath;
            Target = target;
            Alias = alias;
        }

        public ref readonly NodeList<ImportDeclarationSpecifier> Specifiers => ref _specifiers;

        public override NodeCollection ChildNodes => GenericChildNodeYield.Yield(_specifiers, Target, Alias);

        protected internal override void Accept(AstVisitor visitor)
        {
            visitor.VisitImportDeclaration(this);
        }
    }
}
