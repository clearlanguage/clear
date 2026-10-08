#pragma once

#include <filesystem>
#include <optional>
#include <string>
#include <vector>

// Clear's package manager. A project is a directory with a clear.toml:
//
//     [package]
//     name = "app"
//     main = "main.cl"            # the program (default main.cl)
//     lib  = "app.cl"             # what `import "app"` gives other projects (default <name>.cl, lib.cl or main.cl)
//
//     [dependencies]
//     colors = { git = "https://github.com/someone/colors", tag = "v1.2" }   # or branch = / rev =
//     shapes = { path = "../shapes" }
//
// `clearc fetch` clones git dependencies (and theirs) into .clear/packages and records the exact
// commits in clear.lock, so every later build uses the same code until `clearc update`.
// `import "colors"` then reads the package's lib file, `import "colors/extra"` another file of it.

namespace clear
{
    struct Dependency
    {
        std::string Name;
        std::string Git;
        std::string Tag, Branch, Rev;
        std::filesystem::path Path;
    };

    struct Manifest
    {
        std::filesystem::path Root;
        std::string Name;
        std::string Version = "0.1.0";
        std::filesystem::path Main = "main.cl";
        std::filesystem::path Lib;
        std::vector<Dependency> Dependencies;

        static std::optional<Manifest> Load(const std::filesystem::path& directory, std::string& error);
        std::filesystem::path LibraryFile() const;
    };

    // a resolved package: its name and the directory its files live in
    struct Package
    {
        std::string Name;
        std::filesystem::path Directory;
        std::filesystem::path Entry;
    };

    namespace PackageManager
    {
        bool IsProject(const std::filesystem::path& directory);

        int NewProject(const std::filesystem::path& directory);
        int AddDependency(const std::filesystem::path& projectDirectory, const Dependency& dependency);

        // makes every dependency available locally; update ignores clear.lock and takes the newest matching commits
        std::optional<std::vector<Package>> Fetch(const Manifest& manifest, bool update, bool verbose, std::string& error);
    }
}
