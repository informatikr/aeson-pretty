#!/usr/bin/env python3
"""
Makes sure:

 * All jobs are listed in the 'all' job
 * Only existing tests are listed

Copied from: https://github.com/bittide/bittide-hardware/blob/478f300a7a3e27028317cb7e3fadf4090821974e/.github/scripts/all_check.py
"""

# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

import sys
import yaml

CI_PATH = ".github/workflows/ci.yml"
ALL_TEST = "all"

def main():
  ci_yml_fp = open(CI_PATH, "r")
  ci_yml_parsed = yaml.load(ci_yml_fp, Loader=yaml.FullLoader)

  all_jobs = set(ci_yml_parsed['jobs'].keys()) - {ALL_TEST}
  all_needs = set(ci_yml_parsed["jobs"][ALL_TEST]["needs"])

  if all_jobs - all_needs:
    sys.exit(f"Not all jobs mentioned in {ALL_TEST}.needs: {all_jobs - all_needs}")

  if all_needs - all_jobs:
    sys.exit(f"Non-existing jobs found in {ALL_TEST}.needs: {all_needs - all_jobs}")


if __name__ == '__main__':
  main()
