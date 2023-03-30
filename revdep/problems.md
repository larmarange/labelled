# ggformula

<details>

* Version: 0.10.2
* GitHub: https://github.com/ProjectMOSAIC/ggformula
* Source code: https://github.com/cran/ggformula
* Date/Publication: 2022-09-01 00:00:02 UTC
* Number of recursive dependencies: 128

Run `revdepcheck::revdep_details(, "ggformula")` for more info

</details>

## Newly broken

*   checking for code/documentation mismatches ... WARNING
    ```
    Erreurs 'codoc' pour la documentation de l'objet 'var_label' :
    var_label
      Code : function(x, ...)
      Docs : function(x, unlist = FALSE)
      Noms d'arguments dans le code mais pas dans la doc :
        ...
      Noms d'arguments dans la doc mais pas dans le code :
        unlist
      Incohérence dans le nom des arguments :
        Position: 2 Code: ... Docs: unlist
    ...
        Position: 2 Code: ... Docs: unlist
    var_label
      Code : function(x, ...)
      Docs : function(x, unlist = FALSE)
      Noms d'arguments dans le code mais pas dans la doc :
        ...
      Noms d'arguments dans la doc mais pas dans le code :
        unlist
      Incohérence dans le nom des arguments :
        Position: 2 Code: ... Docs: unlist
    ```

## In both

*   checking Rd cross-references ... WARNING
    ```
    Packages non disponibles pour vérifier les xrefs Rd : 'interp', 'quantreg'
    ```

