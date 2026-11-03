#include <iostream>
#include <vector>
#include <unordered_map>
#include <cstdint>
#include <cmath>
#include <utility>
#include <chrono>
#include <thread>
#include <fstream>
#include <iomanip>
#include <boost/multiprecision/cpp_int.hpp>



[[noreturn]] void freezeOnCounterexample(){
    for (;;) std::this_thread::sleep_for(std::chrono::hours(24));
}


//uint64 has an upper bound on the range of numbers it can store. As such, it's necessary 
//to employ a different standard to handle numbers outside the range of uint64
bool complexVerify(boost::multiprecision::cpp_int num){

    const boost::multiprecision::cpp_int initNum = num;

    if ((num & 1) == 0) num /= 2;
    else num = (num * 3) + 1;
    while (initNum < num) {
        if ((num & 1) == 0) num /= 2;
        else num = (num * 3) + 1;
    }
    if (num == initNum) {
        std::cout << "The number: " << initNum << "appears to be a counterexample";
        std::cout.flush();
        freezeOnCounterexample();
    }

    return true;
}

bool simpleVerify(uint64_t num){
    uint64_t initNum = num;
    if((num & 1) == 0) num = num/2;
    else {
        if (num > (UINT64_MAX - 1) / 3){
            return complexVerify(initNum);
        }
        num = (num * 3) + 1;
    }
    while(initNum<num){
        if((num & 1) == 0) num = num/2;
        else {
            if (num > (UINT64_MAX - 1) / 3){
                return complexVerify(initNum);
            }
            num = (num * 3) + 1;
        }
    }

    if(num == initNum){
        std::cout << "The number: " << initNum << "appears to be a counterexample";
        std::cout.flush();
        freezeOnCounterexample();
    }
    return true;
};

uint64_t powerOfTwo(uint64_t exponent) {
    return UINT64_C(1) << exponent;
}

struct TableSnapshot {
    uint64_t exponent;
    uint64_t powerOfTwo;
    size_t tableSize;
    long double percentageImprovement;
    long double speedUp;
    long double logSpeedUp;
    long double totalComputationMilliseconds;
    long double tableComputationMilliseconds;
};

std::vector<TableSnapshot>& generatedTables() {
    static std::vector<TableSnapshot> tables;
    return tables;
}

struct TableEntry {
    uint64_t key;
    uint64_t first;
    uint64_t second;
};

std::unordered_map<uint64_t, std::pair<uint64_t, uint64_t>> vectorToHashMap(
    const std::vector<TableEntry>& entries) {
    std::unordered_map<uint64_t, std::pair<uint64_t, uint64_t>> map;
    map.reserve(entries.size());
    for (const auto& entry : entries) {
        map.emplace(entry.key, std::make_pair(entry.first, entry.second));
    }
    return map;
}

class prunnedEquivalenceTable{
    public:
    uint64_t exponent;
    std::vector<TableEntry> table;
    std::chrono::steady_clock::duration totalComputationTime{};
    std::chrono::steady_clock::duration tableComputationTime{};
    
        prunnedEquivalenceTable(uint64_t exp){
            exponent = exp;

            std::chrono::steady_clock::duration recursiveComputationTime{};
            auto tableComputationStart = std::chrono::steady_clock::now();

            uint64_t powerOfExp = powerOfTwo(exp);
            if(exp>0){
                uint64_t powerOfPrevExp = powerOfTwo(exp - 1);
                

                prunnedEquivalenceTable prevPrunnedTable = prunnedEquivalenceTable(exp - 1);

                recursiveComputationTime = prevPrunnedTable.totalComputationTime;
                tableComputationStart = std::chrono::steady_clock::now();

                std::vector<TableEntry> prevTable = std::move(prevPrunnedTable.table);
                table.reserve(prevTable.size()*2);

                for(const auto& [key, first, second]: prevTable){
                    
                    
                    if((second & 1) == 0){
                        
                        uint64_t pseudosecond = second/2;
                        if(first < powerOfExp){ 
                            //this condition imples that the child class must be prunned, before prunning, we must verify that all members below a certain value descend to one
                            //std::cout << "first insertion got prunned" << std::endl;
                            if(pseudosecond>key){
                                for(uint64_t i = 0; i<=(pseudosecond - key)/(powerOfExp-first); i++){
                                    simpleVerify(powerOfExp*i+key);
                                };
                            }
                        } else {
                            //if the child class does not not need to be prunned, its inserted into the table
                            table.push_back({key, first, pseudosecond});
                        }
                    } else{
                        //if(anomalous){std::cout << "odd step" << std::endl;}
                        table.push_back({key, first*3, (second*3 + 1)/2});
                    }
                    
                    if(((first+second) & 1) == 0){
                        
                        uint64_t pseudosecond = (second + first)/2;

                        if(first < powerOfExp){
                            
                            if(pseudosecond>(key + powerOfPrevExp)){
                                for(uint64_t i = 0; i<=(pseudosecond - (key + powerOfPrevExp))/(powerOfExp-first); i++){
                                    simpleVerify(powerOfExp*i+key + powerOfPrevExp);
                                }
                            }
                        } else {
                            table.push_back({key + powerOfPrevExp, first, pseudosecond});
                        }
                    } else{
                        table.push_back({key + powerOfPrevExp, first*3, ((second + first)*3+1)/2});
                    }
                }
            } else{
                table.push_back({0, 1, 0});
            }

            //this part mostly consists of data used for analytics of code performance
            tableComputationTime = std::chrono::steady_clock::now() - tableComputationStart;
            totalComputationTime = recursiveComputationTime + tableComputationTime;
            generatedTables().push_back({
                exp,
                powerOfExp,
                table.size(),
                static_cast<long double>(table.size()) / powerOfExp,
                static_cast<long double>(powerOfExp) / table.size(),
                static_cast<long double>(std::log((powerOfExp) / table.size())),
                std::chrono::duration<long double, std::milli>(totalComputationTime).count(),
                std::chrono::duration<long double, std::milli>(tableComputationTime).count()
            });
            std::cout << "table of exp: " << exp << std::endl;
            std::cout << "table size:" << table.size() << std::endl;
            std::cout << "table exponenet:" << powerOfExp << std::endl;
            std::cout << "percentage improvement: " << static_cast<long double>(table.size()) / powerOfExp << std::endl;
            std::cout << "speed-up: " << static_cast<long double>(powerOfExp) / table.size() << std::endl;
            std::cout << "log speed-up: " << static_cast<long double>(std::log((powerOfExp) / table.size())) << std::endl;
            
            std::cout << "total computation time: "
                      << std::chrono::duration<long double, std::milli>(totalComputationTime).count()
                      << " ms" << std::endl;
            std::cout << "this table computation time: "
                      << std::chrono::duration<long double, std::milli>(tableComputationTime).count()
                      << " ms" << std::endl;
            
        }
};

bool exportTablesToJson(const char* fileName) {
    std::ofstream output(fileName);
    if (!output) return false;

    output << std::setprecision(21);
    output << "{\n  \"tables\": [\n";

    const auto& tables = generatedTables();
    for (size_t i = 0; i < tables.size(); ++i) {
        const auto& table = tables[i];
        output << "    {\n"
               << "      \"exponent\": " << table.exponent << ",\n"
               << "      \"power_of_two\": " << table.powerOfTwo << ",\n"
               << "      \"table_size\": " << table.tableSize << ",\n"
               << "      \"percentage_improvement\": " << table.percentageImprovement << ",\n"
               << "      \"speed_up\": " << table.speedUp << ",\n"
               << "      \"total_computation_ms\": " << table.totalComputationMilliseconds << ",\n"
               << "      \"this_table_computation_ms\": " << table.tableComputationMilliseconds << "\n"
               << "    }";
        if (i + 1 < tables.size()) output << ',';
        output << '\n';
    }

    output << "  ]\n}\n";
    return output.good();
}



int main() {
    uint64_t exp = 32;
    prunnedEquivalenceTable prunnedEquivalenceTable(exp);
    return exportTablesToJson("prunned_tables.json") ? 0 : 1;
}