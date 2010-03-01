{application, mprocreg, [
    {description, "MProcReg - Mnesia-based global process registry."},
    {vsn, "0.1"},
    {modules, [
        mprocreg_app,
        mprocreg_sup,
        mprocreg
    ]},
    {applications, [kernel, stdlib, mnesia]},
    {mod, {mprocreg_app, []}}
]}.
