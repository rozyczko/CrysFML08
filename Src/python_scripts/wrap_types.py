def wrap(modules) -> None:

    if not os.path.isdir('CFML_Wraps'):
        os.mkdir('CFML_Wraps')
    for m in modules:
        wrap_cfml_module_types(m)
    write_cfml_wraps()
    return None