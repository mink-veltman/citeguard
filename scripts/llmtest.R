llm_model('openai/gpt-oss-20b')
llm_use(TRUE)


text = 'hi'
query = 'reply (keep output as short as possible, max 3 characters)'

llm(text, query)