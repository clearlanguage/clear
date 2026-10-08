#include "PackageManager.h"

#include <llvm/ADT/StringRef.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/Program.h>
#include <toml++/toml.h>

#include <fstream>
#include <map>
#include <print>
#include <sstream>

namespace clear
{
    static const char* s_ManifestName = "clear.toml";
    static const char* s_LockName = "clear.lock";

    static std::string Describe(const Dependency& dependency)
    {
        if (!dependency.Path.empty())
            return "path " + dependency.Path.string();

        std::string where = dependency.Git;
        if (!dependency.Tag.empty())    where += " tag " + dependency.Tag;
        if (!dependency.Branch.empty()) where += " branch " + dependency.Branch;
        if (!dependency.Rev.empty())    where += " rev " + dependency.Rev;
        return where;
    }

    std::optional<Manifest> Manifest::Load(const std::filesystem::path& directory, std::string& error)
    {
        std::filesystem::path file = directory / s_ManifestName;
        toml::table table;

        try
        {
            table = toml::parse_file(file.string());
        }
        catch (const toml::parse_error& parseError)
        {
            error = std::format("{}: {}", file.string(), parseError.description());
            return std::nullopt;
        }

        Manifest manifest;
        manifest.Root = std::filesystem::absolute(directory);
        manifest.Name = table["package"]["name"].value_or(manifest.Root.filename().string());
        manifest.Version = table["package"]["version"].value_or(manifest.Version);
        manifest.Main = table["package"]["main"].value_or(manifest.Main.string());
        manifest.Lib = table["package"]["lib"].value_or(std::string());

        if (auto dependencies = table["dependencies"].as_table())
        {
            for (auto& [key, value] : *dependencies)
            {
                Dependency dependency;
                dependency.Name = std::string(key.str());

                if (auto url = value.value<std::string>())
                {
                    dependency.Git = *url; // name = "https://..." is short for { git = "..." }
                }
                else if (auto details = value.as_table())
                {
                    dependency.Git = (*details)["git"].value_or(std::string());
                    dependency.Tag = (*details)["tag"].value_or(std::string());
                    dependency.Branch = (*details)["branch"].value_or(std::string());
                    dependency.Rev = (*details)["rev"].value_or(std::string());
                    dependency.Path = (*details)["path"].value_or(std::string());
                }

                if (dependency.Git.empty() == dependency.Path.empty())
                {
                    error = std::format("{}: dependency '{}' needs either git = \"<url>\" or path = \"<directory>\"", file.string(), dependency.Name);
                    return std::nullopt;
                }

                manifest.Dependencies.push_back(dependency);
            }
        }

        return manifest;
    }

    std::filesystem::path Manifest::LibraryFile() const
    {
        if (!Lib.empty())
            return Root / Lib;

        for (const auto& candidate : { std::filesystem::path(Name + ".cl"), std::filesystem::path("lib.cl"), Main })
        {
            if (std::filesystem::exists(Root / candidate))
                return Root / candidate;
        }

        return Root / (Name + ".cl");
    }

    // runs git with the given arguments; the first line it prints goes to `output`
    static bool Git(const std::vector<std::string>& arguments, std::string* output = nullptr)
    {
        auto git = llvm::sys::findProgramByName("git");

        if (!git)
            return false;

        std::vector<llvm::StringRef> argv = { *git };
        for (auto& argument : arguments)
            argv.push_back(argument);

        llvm::SmallString<128> captured;
        llvm::sys::fs::createTemporaryFile("clear-git", "txt", captured);
        std::string capturedPath(captured.str());

        std::optional<llvm::StringRef> redirects[] = { std::nullopt, llvm::StringRef(capturedPath), std::nullopt };
        int status = llvm::sys::ExecuteAndWait(*git, argv, std::nullopt, redirects);

        if (output)
        {
            std::ifstream stream(capturedPath);
            std::getline(stream, *output);
        }

        std::filesystem::remove(capturedPath);
        return status == 0;
    }

    namespace PackageManager
    {
        bool IsProject(const std::filesystem::path& directory)
        {
            std::error_code ec;
            return std::filesystem::is_directory(directory, ec) && std::filesystem::exists(directory / s_ManifestName, ec);
        }

        int NewProject(const std::filesystem::path& directory)
        {
            if (std::filesystem::exists(directory / s_ManifestName))
            {
                std::println(stderr, "clearc: {} already has a {}", directory.string(), s_ManifestName);
                return 1;
            }

            std::filesystem::create_directories(directory);
            std::string name = std::filesystem::absolute(directory).filename().string();

            std::ofstream(directory / s_ManifestName) << std::format(
                "[package]\nname = \"{}\"\nversion = \"0.1.0\"\nmain = \"main.cl\"\n\n[dependencies]\n"
                "# colors = {{ git = \"https://github.com/someone/colors\", tag = \"v1.0\" }}\n"
                "# shapes = {{ path = \"../shapes\" }}\n", name);

            if (!std::filesystem::exists(directory / "main.cl"))
            {
                std::ofstream(directory / "main.cl") << 
                    "function main() -> int32:\n"
                    "    print(\"hello from " << name << "\")\n"
                    "    return 0\n";
            }

            std::ofstream(directory / ".gitignore") << ".clear/\nbuild/\n";

            std::println("Created project '{}' in {}", name, directory.string());
            std::println("  clearc run {}", directory.string());
            return 0;
        }

        int AddDependency(const std::filesystem::path& projectDirectory, const Dependency& dependency)
        {
            std::string error;
            auto manifest = Manifest::Load(projectDirectory, error);

            if (!manifest)
            {
                std::println(stderr, "clearc: {}", error);
                return 1;
            }

            for (auto& existing : manifest->Dependencies)
            {
                if (existing.Name == dependency.Name)
                {
                    std::println(stderr, "clearc: '{}' is already a dependency ({}); change it in clear.toml", dependency.Name, Describe(existing));
                    return 1;
                }
            }

            // one new line in [dependencies]; the rest of the file (comments, order) stays as the user wrote it
            auto quote = [](const std::string& text)
            {
                std::string quoted = "\"";
                for (char c : text)
                {
                    if (c == '"' || c == '\\') quoted += '\\';
                    quoted += c;
                }
                return quoted + "\"";
            };

            std::string fields;
            auto field = [&](const char* key, const std::string& value)
            {
                if (!value.empty())
                    fields += std::format("{}{} = {}", fields.empty() ? "" : ", ", key, quote(value));
            };

            field("git", dependency.Git);
            field("tag", dependency.Tag);
            field("branch", dependency.Branch);
            field("rev", dependency.Rev);
            field("path", dependency.Path.generic_string());

            std::string line = std::format("{} = {{ {} }}", dependency.Name, fields);

            std::filesystem::path file = projectDirectory / s_ManifestName;
            std::ifstream input(file);
            std::vector<std::string> lines;
            for (std::string text; std::getline(input, text);)
                lines.push_back(text);
            input.close();

            auto isHeader = [](const std::string& text) { return text.find_first_not_of(" \t") != std::string::npos && text[text.find_first_not_of(" \t")] == '['; };
            auto section = std::find_if(lines.begin(), lines.end(), [](const std::string& text) { return text.starts_with("[dependencies]"); });

            if (section == lines.end())
            {
                if (!lines.empty() && !lines.back().empty())
                    lines.push_back("");
                lines.push_back("[dependencies]");
                lines.push_back(line);
            }
            else
            {
                // after the last entry of the section (before the next [header])
                auto end = std::find_if(std::next(section), lines.end(), isHeader);
                auto insertAt = end;
                while (insertAt != std::next(section) && std::prev(insertAt)->find_first_not_of(" \t") == std::string::npos)
                    insertAt--;
                lines.insert(insertAt, line);
            }

            std::ofstream output(file);
            for (auto& text : lines)
                output << text << "\n";

            std::println("Added {} ({})", dependency.Name, Describe(dependency));
            return 0;
        }

        static std::map<std::string, std::string> LoadLock(const std::filesystem::path& root)
        {
            std::map<std::string, std::string> commits;

            try
            {
                toml::table table = toml::parse_file((root / s_LockName).string());

                if (auto packages = table["package"].as_array())
                {
                    for (auto& node : *packages)
                    {
                        if (auto package = node.as_table())
                            commits[(*package)["name"].value_or(std::string())] = (*package)["commit"].value_or(std::string());
                    }
                }
            }
            catch (...)
            {
            }

            return commits;
        }

        std::optional<std::vector<Package>> Fetch(const Manifest& manifest, bool update, bool verbose, std::string& error)
        {
            std::map<std::string, std::string> locked = update ? std::map<std::string, std::string>{} : LoadLock(manifest.Root);
            std::map<std::string, std::string> commits;
            std::map<std::string, std::string> sources;
            std::vector<Package> packages;

            // breadth first: the project's own dependencies, then theirs
            std::vector<std::pair<Dependency, std::filesystem::path>> queue;
            for (auto& dependency : manifest.Dependencies)
                queue.emplace_back(dependency, manifest.Root);

            for (size_t i = 0; i < queue.size(); i++)
            {
                auto [dependency, base] = queue[i];
                std::string source = dependency.Path.empty() ? Describe(dependency) : std::filesystem::weakly_canonical(base / dependency.Path).string();

                if (auto seen = sources.find(dependency.Name); seen != sources.end())
                {
                    if (seen->second != source)
                    {
                        error = std::format("two different packages are both called '{}': {} and {}", dependency.Name, seen->second, source);
                        return std::nullopt;
                    }

                    continue;
                }

                sources[dependency.Name] = source;
                std::filesystem::path directory;

                if (!dependency.Path.empty())
                {
                    directory = std::filesystem::weakly_canonical(base / dependency.Path);

                    if (!std::filesystem::is_directory(directory))
                    {
                        error = std::format("dependency '{}': {} is not a directory", dependency.Name, directory.string());
                        return std::nullopt;
                    }
                }
                else
                {
                    directory = manifest.Root / ".clear" / "packages" / dependency.Name;
                    bool fresh = !std::filesystem::exists(directory / ".git");

                    if (fresh)
                    {
                        std::println("  fetching {} from {}", dependency.Name, dependency.Git);
                        std::filesystem::create_directories(directory.parent_path());

                        if (!Git({ "clone", "--quiet", dependency.Git, directory.string() }))
                        {
                            std::filesystem::remove_all(directory);
                            error = std::format("could not clone '{}' from {} (is git installed and the URL reachable?)", dependency.Name, dependency.Git);
                            return std::nullopt;
                        }
                    }
                    else if (update)
                    {
                        Git({ "-C", directory.string(), "fetch", "--quiet", "--tags", "origin" });
                    }

                    // the locked commit wins, then rev, tag or branch, else whatever the default branch points at
                    std::string target;
                    if (auto lock = locked.find(dependency.Name); lock != locked.end() && !lock->second.empty()) target = lock->second;
                    else if (!dependency.Rev.empty())    target = dependency.Rev;
                    else if (!dependency.Tag.empty())    target = "tags/" + dependency.Tag;
                    else if (!dependency.Branch.empty()) target = "origin/" + dependency.Branch;
                    else if (update)                     target = "origin/HEAD";

                    std::string current;
                    Git({ "-C", directory.string(), "rev-parse", "HEAD" }, &current);

                    if (!target.empty())
                    {
                        std::string wanted;
                        if (!Git({ "-C", directory.string(), "rev-parse", target + "^{commit}" }, &wanted))
                        {
                            Git({ "-C", directory.string(), "fetch", "--quiet", "--tags", "origin" });

                            if (!Git({ "-C", directory.string(), "rev-parse", target + "^{commit}" }, &wanted))
                            {
                                error = std::format("dependency '{}': {} not found in {}", dependency.Name, target, dependency.Git);
                                return std::nullopt;
                            }
                        }

                        if (wanted != current && !Git({ "-C", directory.string(), "checkout", "--quiet", "--detach", wanted }))
                        {
                            error = std::format("dependency '{}': could not check out {}", dependency.Name, target);
                            return std::nullopt;
                        }
                    }

                    Git({ "-C", directory.string(), "rev-parse", "HEAD" }, &commits[dependency.Name]);

                    if (verbose)
                        std::println("  {} at {}", dependency.Name, commits[dependency.Name].substr(0, 12));
                }

                // a package may be a project of its own, with a lib file and dependencies
                Manifest inner;
                inner.Root = directory;
                inner.Name = dependency.Name;

                if (IsProject(directory))
                {
                    auto loaded = Manifest::Load(directory, error);

                    if (!loaded)
                        return std::nullopt;

                    inner = *loaded;
                    inner.Name = dependency.Name; // imported by the name the depending project gave it

                    for (auto& next : loaded->Dependencies)
                        queue.emplace_back(next, directory);
                }

                packages.push_back(Package { dependency.Name, directory, inner.LibraryFile() });
            }

            // clear.lock: the exact commit of every git package
            if (!commits.empty() || std::filesystem::exists(manifest.Root / s_LockName))
            {
                std::ofstream lock(manifest.Root / s_LockName);
                lock << "# written by clearc fetch: the exact version of every git dependency\n";

                for (auto& [name, commit] : commits)
                    lock << std::format("\n[[package]]\nname = \"{}\"\nsource = \"{}\"\ncommit = \"{}\"\n", name, sources[name], commit);
            }

            return packages;
        }
    }
}
