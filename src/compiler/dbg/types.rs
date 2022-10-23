pub trait MessageHandler<M> {
    fn handle_message(&mut self, msg: &M) -> Result<Option<Vec<M>>, String>;
}
