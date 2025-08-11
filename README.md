# `jsondiff`

[![Github CI](https://github.com/eunikolsky/jsondiff/workflows/CI/badge.svg)](https://github.com/eunikolsky/jsondiff/actions)

## Purpose and usage

`jsondiff` helps with managing translations in localization files, specifically calculating what needs to be translated and integrating new translations. The tool works only with hierarchical key-value JSON files of such a format:

```json
{
  "main": {
    "no": "no",
    "save": "Save",
    "yes": "yes"
  },
  "group": {
    "inside": {
      "group": {
        "cancel": "Cancel"
      },
      "array": ["foo", "bar"]
    },
    "remove": "remove"
  },
  "root": "Root"
}
```

You can get a binary (for macos) from the releases page: <https://github.com/eunikolsky/jsondiff/releases>.

### Generating diff

Given a source language file with all localization strings so far (for example, English) and a destination language file that needs translated strings (for example, Spanish), the first thing to do is to figure out which strings are new and need to be sent for translation; strings already present in the Spanish file should not be sent again (if an original English string has changed since it received a translation, the developer is expected not only to update it, but also remove the corresponding key from the Spanish and other languages since it's now invalid). That is, only the strings that are present in the English file and not present in the Spanish file need to be sent.

To get the diff, provide two filenames:

```bash
$ jsondiff diff --english en.json --translation es.json
```

For example, if `en.json` contains:

```json
{
  "main": {
    "no": "no",
    "save": "Save",
    "yes": "yes"
  },
  "group": {
    "inside": {
      "group": {
        "cancel": "Cancel"
      }
    },
    "remove": "remove"
  },
  "root": "Root"
}
```

and `es.json` contains:

```json
{
  "main": {
    "save": "1234"
  },
  "group": {
    "remove": "abcd"
  }
}
```

the output will be:

```bash
$ jsondiff diff --english en.json --translation es.json
{
  "group": {
    "inside": {
      "group": {
        "cancel": "Cancel"
      }
    }
  },
  "main": {
    "no": "no",
    "yes": "yes"
  },
  "root": "Root"
}
```

### Integrating updates

The source language file is not static and will have modifications for new features, removing old ones, etc. Translation isn't instantaneous, so it's likely that such changes will happen between a translation request is sent and the result is received. Thus the second thing is to correctly integrate the received translations.

There are several types of changes possible, they are described below with examples. To understand and apply the differences, the program needs four files:

1. Old English file — the file that was used to calculate the diff to send for translations. It is important to know the exact version of this file.

2. Current English file — current version of the English file.

3. Current translation file — current version of a translation file (e.g. Spanish).

4. New translations file — the file received from translators, which corresponds to the diff from the old English file.

This allows stateless tracking of changes, which is much more preferred than stateful tracking because we don't need any storage for extra information about changes. This is very similar to how `git` stores complete versions of files and calculates `diff` on demand vs. how `svn` stores diffs from one revision to another.

#### Key-value hasn't changed

This is the simplest case, the Spanish string can be copied as is. For example:

* Old and current English files:

    ```json
    {
      "existing": "existing",
      "key": "value"
    }
    ```

* Current Spanish file:

    ```json
    {
      "existing": "EXISTING"
    }
    ```

* Spanish translation file:

    ```json
    {
      "key": "VALUE"
    }
    ```

The output is:

```
$ jsondiff integrate --old-english old_en.json --current-english en.json --current-translation es.json --new-translation new_es.json
{
  "existing": "EXISTING",
  "key": "VALUE"
}
```

#### Value has changed

When a value for a key has changed, the translation for the old value is invalid for the new value, and so it is ignored. Example:

* Old English file:

    ```json
    {
      "existing": "existing",
      "key": "value"
    }
    ```

* Current English file:

    ```json
    {
      "existing": "existing",
      "key": "value changed"
    }
    ```

* Current Spanish file:

    ```json
    {
      "existing": "EXISTING"
    }
    ```

* Spanish translation file:

    ```json
    {
      "key": "VALUE"
    }
    ```

The output is:

```
{
  "existing": "EXISTING"
}

These keys were ignored because their values changed since the translation was sent:
key: "value" => "value changed"
```

Note that the program first outputs the resulting json file to `stdout`, then outputs all warnings to `stderr`.

#### Key was removed

The original English string was removed, so there is no need to copy its Spanish translation. Example:

* Old English file:

    ```json
    {
      "existing": "existing",
      "key": "value"
    }
    ```

* Current English file:

    ```json
    {
      "existing": "existing"
    }
    ```

* Current Spanish file:

    ```json
    {
      "existing": "EXISTING"
    }
    ```

* Spanish translation file:

    ```json
    {
      "key": "VALUE"
    }
    ```

The output is:

```
{
  "existing": "EXISTING"
}

These keys were ignored because they were removed since the translation was sent:
key: "value"
```

#### Value was moved to another key

This is a more specific variant of the previous case: a key was removed, but its value is now present at another key (or multiple keys). The Spanish translation is copied to all the other keys. Example:

* Old English file:

    ```json
    {
      "existing": "existing",
      "key": "value"
    }
    ```

* Current English file:

    ```json
    {
      "existing": "existing",
      "new_key": "value",
      "group": {
        "another_value": "value"
      }
    }
    ```

* Current Spanish file:

    ```json
    {
      "existing": "EXISTING"
    }
    ```

* Spanish translation file:

    ```json
    {
      "key": "VALUE"
    }
    ```

The output is:

```
{
  "existing": "EXISTING",
  "new_key": "VALUE",
  "group": {
    "another_value": "VALUE"
  }
}

Values for these keys were applied at new keys:
key => [group.another_value,new_key]
```

#### Unknown translated value

If a key wasn't present in the old English file and is present in the new translation file, it's ignored because its origin is unidentified. Example:

* Old and current English files:

    ```json
    {
      "existing": "existing",
      "key": "value"
    }
    ```

* Current Spanish file:

    ```json
    {
      "existing": "EXISTING"
    }
    ```

* Spanish translation file:

    ```json
    {
      "unknown": "UNKNOWN"
    }
    ```

The output is:

```
{
  "existing": "EXISTING"
}

These keys were ignored because they were missing in the old English file:
unknown: "UNKNOWN"
```

#### Type change for a key

If a key's type has changed since the translation was sent, the program will try to merge the `json`s preferring objects and will log type conflicts. Example:

```bash
$ jsondiff integrate \
  --old-english <( echo '{"key":"value"}' ) \
  --current-english <( echo '{"key":{"nested":"value"}}' ) \
  --current-translation <( echo '{"key":""}' ) \
  --new-translation <( echo '{"key":"translated"}' )
TYPE CONFLICT: key='key' string vs object (preferring object)
{
  "key": {
    "nested": "translated"
  }
}

Values for these keys were applied at new keys:
key => [key.nested]
```

## Building

[Haskell `stack`](https://docs.haskellstack.org/en/stable/README/) is used to manage the project.

* Build the project with `stack build`.

* Install the compiled executable with `stack install jsondiff`.

* Run tests with `stack test`.

## Known issues

* The program expects JSON files only with objects with types of values `string`, `array of strings` and `object`s with the same restrictions recursively. Encountering any other type will terminate the program:

    ```
    $ jsondiff diff --english <( echo '{"key":"value", "incorrect":42, "nested": {"null":null} }' ) --translation <( echo '{}' )
    jsondiff: Unexpected null at ["nested","null"]
    CallStack (from HasCallStack):
      error, called at src/Lib.hs:49:36 in jsondiff-0.2.0.0-v9eJp9A7FSB7sAvbhY2bS:Lib
      unexpectedType, called at src/Lib.hs:46:23 in jsondiff-0.2.0.0-v9eJp9A7FSB7sAvbhY2bS:Lib
    ```
