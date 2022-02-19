import json
import subprocess
import sys
import argparse
import os

verbose=False

config_file = "config/config.json"
benchmarks_config_file = "config/benchmarks.json"
qc_config = "config/qc_zero_knowledge_conf.json"
cmd_prefix = ["dune", "exec", "--", "main/main.exe"]

def solve_tap(p_setting):
    target_file = "/".join([p_setting['prefix'], p_setting['name'], p_setting['target']])
    assertion_file = "/".join([p_setting['prefix'], p_setting['name'], p_setting['assert']])
    pf_file = "/".join([p_setting['prefix'], p_setting['name'], p_setting['pf']])
    return target_file, assertion_file, pf_file

def invoc_cmd(cmd, output_file):
    print(" ".join(cmd))
    if (verbose):
        print(" ".join(cmd))
    if outfile:
        with open(output_file, "w") as ofile:
            subprocess.run(cmd, stdout=ofile)
    else:
        subprocess.run(cmd)

def syn(p_setting, num, time, output_file):
    target_file, assertion_file, _ = solve_tap(p_setting)
    cmd = cmd_prefix + ["synthesize-time", config_file, target_file, assertion_file, num, time]
    invoc_cmd(cmd, output_file)

def eval_pf_time(p_setting, pf_file, time):
    target_file, assertion_file, pf_file_default = solve_tap(p_setting)
    if pf_file == None:
        pf_file = pf_file_default
    cmd = cmd_prefix + ["sampling-time", config_file, target_file, assertion_file, pf_file_default, time]
    invoc_cmd(cmd, None)

def eval_baseline_time(p_setting, qc_config, time):
    target_file, assertion_file, _ = solve_tap(p_setting)
    cmd = cmd_prefix + ["baseline-time", config_file, target_file, assertion_file, qc_config, time]
    invoc_cmd(cmd, None)

def eval_pf_num(p_setting, pf_file, num):
    target_file, assertion_file, pf_file_default = solve_tap(p_setting)
    if pf_file == None:
        pf_file = pf_file_default
    cmd = cmd_prefix + ["eval-sampling", config_file, target_file, assertion_file, pf_file_default, num]
    invoc_cmd(cmd, None)

def eval_baseline_num(p_setting, qc_config, num):
    target_file, assertion_file, pf_file_default = solve_tap(p_setting)
    cmd = cmd_prefix + ["baseline", config_file, target_file, assertion_file, qc_config, num]
    invoc_cmd(cmd, None)

if __name__ == "__main__":
    # print(os.path.isfile("zhouzhe"))
    with open(benchmarks_config_file) as f:
        bconfig = json.load(f)
    bconfig = bconfig["benchmarks"]
    parser = argparse.ArgumentParser()
    parser.add_argument("action", type=str,
                        help="the expected you want: syn | evalpf | evalqc")
    parser.add_argument("benchmarks", type=str,
                        help="the benchmarks you want to run: rbset | rbset,leftisthp | all")
    parser.add_argument("timebound", nargs='?', type=int, default=300,
                        help="the time bound in second, default is 300s")
    parser.add_argument("sizebound", nargs='?', type=int, default=30,
                        help="the size bound, default is 30")
    parser.add_argument("pffile", nargs='?', type=str, default=None,
                        help="the pf file")
    parser.add_argument("outfile", nargs='?', type=str, default=None,
                        help="the output file")
    parser.add_argument("-v", "--verbose", action="store_true",
                        help="show executing commands")
    args = parser.parse_args()
    action = args.action
    timebound = str(args.timebound)
    sizebound = str(args.sizebound)
    verbose=args.verbose
    outfile=args.outfile
    prefix = bconfig['prefix']
    for x in bconfig['meta']:
        x['prefix'] = bconfig['prefix']
    if args.benchmarks == "all":
        bs = bconfig['meta']
    else:
        names = args.benchmarks.split(',')
        bs = []
        for name in names:
            try:
                result = next(x for x in bconfig['meta'] if x['name'] == name)
                bs.append(result)
            except:
                print("cannot find the config of benchmark {}!".format(name))
                exit()
    if action == "syn":
        for b in bs:
            syn(b, "1", timebound, outfile)
    if action == "evalpf":
        for b in bs:
            eval_pf_time(b, args.pffile, timebound)
    if action == "evalbaseline":
        for b in bs:
            eval_baseline_time(b, qc_config, timebound)
    if action == "evalpfnum":
        for b in bs:
            eval_pf_num(b, args.pffile, sizebound)
    if action == "evalbaselinenum":
        for b in bs:
            eval_baseline_num(b, qc_config, sizebound)

# dune exec -- main/main.exe synthesize-time config/config.json data/client/rbset/balance.ml data/client/rbset/assertion1.ml 1 300 > .prog
# dune exec -- main/main.exe sampling-time config/config.json data/client/rbset/balance.ml data/client/rbset/assertion1.ml .prog 5
# dune exec -- main/main.exe baseline-time config/config.json data/client/rbset/balance.ml data/client/rbset/assertion1.ml config/qc_zero_knowledge_conf.json 5
