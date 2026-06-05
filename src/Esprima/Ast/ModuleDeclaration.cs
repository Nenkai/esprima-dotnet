using System;
using Esprima.Utils;

namespace Esprima.Ast
{
    public sealed class ModuleDeclaration : Declaration
    {
        public readonly Expression? Id; // Identifier || StaticIdentifier
        public readonly Statement Body;

        public ModuleDeclaration(Expression? id, Statement body) :
            base(Nodes.ModuleDeclaration)
        {
            Id = id;
            Body = body;
        }

        public override NodeCollection ChildNodes => new(Id, Body);

        protected internal override void Accept(AstVisitor visitor)
        {
            throw new NotImplementedException();
        }
    }
}
