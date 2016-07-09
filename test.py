def validateRule(rule):
        name     = rule.__class__.name
        mapping  = rule.mapping
        extras   = rule.extras
        defaults = rule.defaults
        exported = rule.exported
        context  = rule.context

        # Type checking of initialization values.
        assert isinstance(name, (str, unicode))
        assert isinstance(mapping, dict)
        for key, value in mapping.iteritems():
            assert isinstance(key, (str, unicode))
        assert isinstance(extras, (list, tuple))
        for item in extras:
            assert isinstance(item, ElementBase)
        assert exported == True or exported == False

        for spec, value in self._mapping.iteritems():
            c = Compound(spec, elements=self._extras, value=value)
            children.append(c)
