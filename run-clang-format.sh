find . \( -iname "*.h" -o -iname "*.cpp" \) -not -path "./ThirdParty/*" -not -path "./Include/WAVM/Inline/xxhash/*" | xargs clang-format-9 -i