module.exports = {
  docs: [
    'README',
    'motivation',
    'tree',
    'yaml',
    'hocon',
    {
      type: 'category',
      label: 'Psi',
      link: { type: 'doc', id: 'psi/README' },
      items: [
        'psi/syntax',
        'psi/merging',
        'psi/substitutions',
        'psi/directives',
        'psi/errors',
        'psi/caveats',
        'psi/api',
        'psi/extending',
      ],
    },
  ],
};
