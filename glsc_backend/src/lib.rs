use cranelift::codegen::Context;
use cranelift::prelude::*;
use cranelift_module::Module;
use cranelift_object::{ObjectBuilder, ObjectModule};
use target_lexicon::BinaryFormat;
use glsc_mir as mir;
pub struct Backend {
    pub module: ObjectModule,
    pub ctx: Context,
}
impl Backend {
    pub fn new() -> Self {
        let mut build_flags = cranelift::codegen::settings::builder();
        build_flags.set("opt_level", "speed_and_size").unwrap();
        build_flags.enable("enable_alias_analysis").unwrap();

        let flags = settings::Flags::new(build_flags);

        let mut target = target_lexicon::triple!("x86_64");
        target.binary_format = BinaryFormat::Elf;

        let isa_builder = cranelift_codegen::isa::lookup(target).unwrap();
        let isa = isa_builder.finish(flags).unwrap();

        let builder =
            ObjectBuilder::new(isa, "program", cranelift_module::default_libcall_names()).unwrap();

        let mut module = ObjectModule::new(builder);
        let mut ctx = module.make_context();

        Self { ctx, module }
    }
    pub fn compile(&mut self, mir: &glsc_mir_lower::MirLower) {
        
    }
}
