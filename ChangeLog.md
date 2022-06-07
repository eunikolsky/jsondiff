# Changelog for `jsondiff`

## 0.2.1

* Fix Unicode strings output in the warnings. For example, the output:

    ```
    These keys were ignored because their values changed since the translation was sent:
    key: "value\10067" => ["value", "changed\8592\10052\65039"]
    ```

    is now shown as:

    ```
    These keys were ignored because their values changed since the translation was sent:
    key: "value❓" => ["value", "changed←❄️"]
    ```

## 0.2.0

* Generate diffs for translation with `jsondiff diff`.
* Integrate translation updates with `jsondiff integrate`.
