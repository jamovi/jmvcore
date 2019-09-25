
#include <Rcpp.h>
#include <vector>

using namespace Rcpp;
using namespace std;

class R6List {
public:
    R6List() {
        // _names.reserve(20000);
        // _items.reserve(20000);
    }
    void append(String name, Environment item) {
        _names.push_back(name);
        _items.push_back(item);
    }
    void insert(int index, String name, Environment item) {
        _names.insert(_names.begin() + index - 1, name);
        _items.insert(_items.begin() + index - 1, item);
    }
    void preallocate(int n, Environment e) {
        _names.reserve(n);
        _items.reserve(n);
    }
    Environment get(String name) {
        int i = 0;
        for (const String &comp : _names) {
            if (comp == name)
                return _items[i];
            i++;
        }
        throw runtime_error("No such name");
    }
    Environment at(int i) {
        return _items[i - 1];
    }
    String nameAt(int i) {
        return _names[i - 1];
    }
    IntegerVector indexOf(String name) {
        int i = 0;
        for (const String &comp : _names) {
            if (comp == name)
                return Rcpp::wrap(i + 1);
            i++;
        }
        return IntegerVector();
    }
    CharacterVector names() const {
        return CharacterVector(_names.begin(), _names.end());
    }
    List items() const {
        return List(_items.begin(), _items.end());
    }
    // R6List clone() const {
    //     R6List copy;
    //     copy._names = vector<String>(_names.begin(), _names.end());
    //     copy._items = vector<Environment>(_items.begin(), _items.end());
    //     return copy;
    // }
    int count() const {
        return _names.size();
    }
    List asList() const {
        List all(_items.begin(), _items.end());
        // all.attr("names") = Rcpp::clone(_names);
        return all;
    }
    void fill(List &all) {
        _items = vector<Environment>(all.begin(), all.end());
        CharacterVector names = all.attr("names");
        _names = vector<String>(names.begin(), names.end());
    }
private:
    vector<String> _names;
    vector<Environment> _items;
};

RCPP_MODULE(R6List) {
    class_<R6List>("R6List")
    .constructor()
    .method("append", &R6List::append)
    .method("get", &R6List::get)
    .method("at", &R6List::at)
    .method("names", &R6List::names)
    .method("items", &R6List::items)
    .method("count", &R6List::count)
    .method("indexOf", &R6List::indexOf)
    .method("preallocate", &R6List::preallocate)
    ;
}
